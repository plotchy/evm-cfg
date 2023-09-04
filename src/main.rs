use crate::cfg_gen::{dasm::InstructionBlock, *};
use clap::{ArgAction, Parser, ValueHint};
use evm_cfg::OutputHandler;
use fnv::FnvBuildHasher;
use revm::Bytecode;
use std::{
    collections::{BTreeMap, HashSet},
    io::Write,
    process::Command,
};

pub mod cfg_gen;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Either a path to a file containing the bytecode or the bytecode itself
    #[clap(value_hint = ValueHint::FilePath, value_name = "PATH or BYTECODE")]
    pub path_or_bytecode: String,

    /// Filename and format for storing the analyzed cfg. Supports all standard
    /// graphviz formats (*.dot, *.png, *.jpg, ...). Default is stdout in dot format
    #[clap(long, short)]
    pub output: Option<String>,

    /// Whether to open the dot visualization of the analyzed cfg as a .svg
    #[clap(long, default_value = "false")]
    pub open: bool,

    /// Verbosity of the cfg creatoor
    ///
    /// Pass multiple times to increase the verbosity (e.g. -v, -vv, -vvv).
    ///
    /// Verbosity levels:
    ///
    ///   0: Print results only (.dot directly to stdout, "saved to <file>")
    ///   1: Additionally print timings
    ///   2: Additionally saves the cfg with only basic connections to cfg_basic_connections.dot
    ///   3: Additionally saves the cfg with only nodes to cfg_nodes_only.dot
    ///   4: Additionally prints all valid jumpdests found by revm
    ///  11: This one goes to 11
    #[clap(long, short, verbatim_doc_comment, action = ArgAction::Count)]
    pub verbosity: u8,
}

fn main() {
    let args = Args::parse();
    let path_string = args.path_or_bytecode;
    // check if path ends with .sol, if so, tell user to use solc to get bytecode and exit
    if path_string.ends_with(".sol") {
        println!("Use solc to get bytecode from solidity source files. ie:");
        println!(
            "   `solc {} --bin-runtime --no-cbor-metadata --optimize --via-ir`",
            &path_string
        );
        println!("then run this tool on the resulting bytecode");
        println!("   `evm_cfg <bytecode>`");
        std::process::exit(1);
    }

    // check if path, if so read file, else use as bytecode
    let bytecode_string = std::fs::read_to_string(&path_string).unwrap_or(path_string);

    // sanitize bytecode string from newlines/spaces/etc
    let bytecode_string = bytecode_string.replace(['\n', ' ', '\r'], "");

    // remove 0x prefix if present
    let bytecode_string = if let Some(stripped) = bytecode_string.strip_prefix("0x") {
        stripped.to_string()
    } else {
        bytecode_string
    };
    let verbosity = args.verbosity;
    let output_handler: OutputHandler = match verbosity {
        0 => OutputHandler::default(),
        1 => OutputHandler {
            show_timings: true,
            ..Default::default()
        },
        2 => OutputHandler {
            show_timings: true,
            show_basic_connections: true,
            ..Default::default()
        },
        3 => OutputHandler {
            show_timings: true,
            show_basic_connections: true,
            show_bare_nodes: true,
            ..Default::default()
        },
        4 => OutputHandler {
            show_timings: true,
            show_basic_connections: true,
            show_bare_nodes: true,
            show_jump_dests: true,
        },
        11 => OutputHandler {
            show_timings: true,
            show_basic_connections: true,
            show_bare_nodes: true,
            show_jump_dests: true,
        },
        _ => OutputHandler {
            show_timings: true,
            show_basic_connections: true,
            show_bare_nodes: true,
            show_jump_dests: true,
        },
    };
    let bytecode_vec = hex::decode(&bytecode_string).expect("bad hex");

    // DISASSEMBLY
    let disassembly_time = std::time::Instant::now();
    // get jumptable from revm
    let revm_bytecode = Bytecode::new_raw(bytecode_vec.clone().into());
    let revm_bytecode = revm_bytecode.lock::<revm::LatestSpec>();
    let revm_jumptable = revm_bytecode.jumptable();

    // convert jumptable to Hashset of valid jumpdests. 2byte key means Fnv is fast
    let set_all_valid_jumpdests: HashSet<u16, FnvBuildHasher> = revm_jumptable
        .analysis
        .iter()
        .enumerate()
        .filter_map(|(pc, data)| {
            if data.is_jump() {
                Some(pc as u16)
            } else {
                None
            }
        })
        .collect();

    if output_handler.show_jump_dests {
        println!("all valid jumpdests: {:?}", &set_all_valid_jumpdests);
    }

    // convert bytecode to instruction blocks
    let mut instruction_blocks = dasm::disassemble(&bytecode_vec);

    // analyze each instruction block statically to determine stack usage agnostic to entry values
    for block in &mut instruction_blocks {
        block.analyze_stack_info();
    }

    // QoL: map cfg-nodes to instruction blocks for easy lookup rather than stuffing graph with instruction block info as node weights
    let mut map_to_instructionblocks: BTreeMap<(u16, u16), InstructionBlock> = instruction_blocks
        .iter()
        .map(|block| ((block.start_pc, block.end_pc), block.clone()))
        .collect();

    // create initial cfg using only nodes
    let mut cfg_runner =
        cfg_gen::cfg_graph::CFGRunner::new(bytecode_vec, &mut map_to_instructionblocks);
    if output_handler.show_bare_nodes {
        // write out the cfg with bare nodes only
        let mut file = std::fs::File::create("cfg_nodes_only.dot").expect("bad fs open");
        file.write_all(cfg_runner.cfg_dot_str_with_blocks().as_bytes())
            .expect("bad file write");
    }

    // form basic edges based on direct pushes leading into jumps, false edges of jumpis, and pc+1 when no jump is used
    cfg_runner.form_basic_connections();
    // trim instruction blocks from graph that have no incoming edges and do not lead the block with a jumpdest
    cfg_runner.remove_unreachable_instruction_blocks();
    if output_handler.show_timings {
        println!("disassembly took: {:?}", disassembly_time.elapsed());
    }

    if output_handler.show_basic_connections {
        // write out the cfg with basic connections only
        let mut file = std::fs::File::create("cfg_basic_connections.dot").expect("bad fs open");
        file.write_all(cfg_runner.cfg_dot_str_with_blocks().as_bytes())
            .expect("bad file write");
    }

    let stack_solve_time = std::time::Instant::now();
    // find new edges based on indirect jumps
    let label_symbolic_jumps = false;
    stack_solve::symbolic_cycle(
        &mut cfg_runner,
        &set_all_valid_jumpdests,
        label_symbolic_jumps,
    );

    if output_handler.show_timings {
        println!("stack_solve took: {:?}", stack_solve_time.elapsed());
    }

    // write out the cfg with found indirect edges
    if let Some(filename) = &args.output {
        let mut file = std::fs::File::create(filename).expect("bad fs open");
        file.write_all(cfg_runner.cfg_dot_str_with_blocks().as_bytes())
            .expect("bad file write");
        println!("Dot file saved to {}", &filename);

        let ext = filename.split('.').last().unwrap();
        if ext != "dot" {
            let output = Command::new("dot")
                .arg(format!("-T{}", ext))
                .arg("-o")
                .arg(filename) // output file
                .arg(filename) // file to read
                .output()
                .expect("failed to execute process");

            if output.stderr.is_empty() {
                println!("File saved to {}", &filename);
            }
        }
    } else {
        println!("{}", cfg_runner.cfg_dot_str_with_blocks());
    };

    if args.open {
        if let Some(filename) = &args.output {
            open::that(filename).expect("failed to open the doc");
        } else {
            eprintln!(
                "Cannot open file that was not saved. Consider specifying output with --output"
            );
        }
    }
}
