use std::{collections::{HashMap, BTreeMap}, hash::Hash, fmt::Debug};
use petgraph::dot::Dot;
use petgraph::prelude::*;
use revm::{Return, opcode::*};
use lazy_static::lazy_static;
use itertools::Itertools;
use crate::cfg_gen::dasm::*;

use super::BLOCK_ENDERS_U8;


lazy_static! {
    pub static ref TOKYO_NIGHT_COLORS: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();
        m.insert("red", "#f7768e");
        m.insert("orange", "#ff9e64");
        m.insert("yellow", "#e0af68");
        m.insert("green", "#9ece6a");
        m.insert("cyan", "#73daca");
        m.insert("teal", "#2ac3de");
        m.insert("darkblue", "#7aa2f7");
        m.insert("purple", "#bb9af7");
        m.insert("bg", "#1a1b26");
        m.insert("font", "#c0caf5");
        m.insert("deepred", "#703440");
        m
    };
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Edges {
    Jump, // Next instruction in sequence
    ConditionTrue, // Conditional jumpi, true branch
    ConditionFalse, // Conditional jumpi, false branch
    SymbolicJump, // Jump to a symbolic value
}

impl Debug for Edges {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Edges::Jump => write!(f, ""),
            Edges::ConditionTrue => write!(f, "True"),
            Edges::ConditionFalse => write!(f, "False"),
            Edges::SymbolicJump => write!(f, "Symbolic"),
        }
    }
}

type CFGDag = GraphMap<(u16,u16), Edges, Directed>;

pub struct CFGRunner<'a> {
    pub cfg_dag: CFGDag,
    pub last_node: Option<(u16,u16)>,
    pub jumpi_edge: Option<Edges>,
    pub bytecode: Vec<u8>,
    pub map_to_instructionblock: &'a mut BTreeMap<(u16,u16), InstructionBlock>,
}

impl<'main> CFGRunner<'main> {

    pub fn new(bytecode: Vec<u8>, map_to_instructionblock: &'main mut BTreeMap<(u16,u16), InstructionBlock>) -> Self {

        let mut cfg_dag: CFGDag = GraphMap::new();
        
        for keys in map_to_instructionblock.keys().sorted_by(|a,b| a.0.cmp(&b.0)) {
            cfg_dag.add_node(*keys);
        }

        Self {
            cfg_dag,
            last_node: None,
            jumpi_edge: None,
            bytecode,
            map_to_instructionblock,
        }
    }

    pub fn initialize_cfg_with_instruction_blocks(&mut self, instruction_blocks: Vec<InstructionBlock>) -> eyre::Result<()> {
        for block in instruction_blocks {
            self.cfg_dag.add_node((block.start_pc, block.end_pc));
        }
        Ok(())
    }

    pub fn form_basic_connections(&mut self) {
        let last_pc_total = self.bytecode.len() as u16;
        /*
        There are 4 cases of edges that we can connect from basic static analysis:
        1. Jumpi false
        2. Jumpi true (direct jump)
        3. Jump (direct jump)
        4. Block ender is a basic instruction (connect to next pc)
            - this happens when a block is broken up by a jumpdest
        */

        // We need to iterate over each of the nodes in the graph, and check the end_pc of the (start_pc, end_pc) node
        for ((_entry_pc, _exit_pc), instruction_block) in self.map_to_instructionblock.iter() {
            let end_pc = instruction_block.end_pc;
            let start_pc = instruction_block.start_pc;
            let last_op = instruction_block.ops.last().unwrap();
            let _last_op_pc = last_op.0;
            let last_op_code = last_op.1;

            let direct_push = &instruction_block.stack_info.push_used_for_jump;
            let direct_push_val = direct_push.as_ref().copied();

            // Case 1: Jumpi false
            if last_op_code == 0x57 {
                // Jumpi false
                let next_pc = end_pc + 1;
                if next_pc >= last_pc_total {
                    // continue;
                } else {
                    let next_node = self.get_node_from_pc(next_pc);
                    self.cfg_dag.add_edge((start_pc, end_pc), next_node, Edges::ConditionFalse);
                }
                
            } 
            if instruction_block.indirect_jump.is_none() && direct_push_val.is_some() {
                // we know this is a direct jump
                // Case 2: Direct Jumpi true
                if last_op_code == 0x57 {
                    // Jumpi true
                    let next_pc = format!("{}", direct_push_val.unwrap()).parse::<u16>().unwrap(); // this is so stupid but its only done once
                    let next_node = self.get_node_from_pc(next_pc);
                    self.cfg_dag.add_edge((start_pc, end_pc), next_node, Edges::ConditionTrue);
                }
    
                // Case 3: Direct Jump
                if last_op_code == 0x56 {
                    // Jump
                    let next_pc = format!("{}", direct_push_val.unwrap()).parse::<u16>().unwrap(); // this is so stupid but its only done once
                    let next_node = self.get_node_from_pc(next_pc);
                    self.cfg_dag.add_edge((start_pc, end_pc), next_node, Edges::Jump);
                }
            }

            if !BLOCK_ENDERS_U8.contains(&last_op_code) && super::opcode(last_op_code).name != "unknown" {
                // Block ender is a basic instruction, but not exiting
                let next_pc = end_pc + 1;
                if self.bytecode.len() < next_pc as usize {
                    continue;
                }
                let next_node = self.get_node_from_pc(next_pc);
                self.cfg_dag.add_edge((start_pc, end_pc), next_node, Edges::Jump);
            }
        }
    }

    pub fn remove_unreachable_instruction_blocks(&mut self) {
        // We need to iterate over the nodes in self.map_to_instructionblock, and remove any that have no incoming/outgoing edges and do not begin with a jumpdest
        let mut to_remove: Vec<(u16,u16)> = Vec::new();
        for ((_entry_pc, _exit_pc), instruction_block) in self.map_to_instructionblock.iter() {
            let start_pc = instruction_block.start_pc;
            let end_pc = instruction_block.end_pc;
            let incoming_edges = self.cfg_dag.edges_directed((start_pc, end_pc), Direction::Incoming);
            if incoming_edges.count() == 0 {
                // This node has no incoming edges, so it is unreachable
                if instruction_block.ops[0].1 != 0x5b && start_pc != 0 {
                    // This node does not begin with a jumpdest, so it is unreachable
                    to_remove.push((start_pc, end_pc));
                }
            }
        }

        // remove the found nodes from the cfg and from the self.map_to_instructionblock
        for node in to_remove {
            self.cfg_dag.remove_node(node);
            self.map_to_instructionblock.remove(&node);
        }
    }

    pub fn get_node_from_pc(&self, pc: u16) -> (u16,u16) {
        for (_key, val) in self.map_to_instructionblock.iter() {
            if val.ops.iter().map(|(instruction_pc, _op, _push_val)| {
                *instruction_pc == pc
            }).any(|x| x) {
                return (val.start_pc, val.end_pc);
            }
        }
        panic!("Could not find node for pc {pc}");
    }

    pub fn get_node_from_entry_pc(&self, pc: u16) -> (u16,u16) {
        for (key, val) in self.map_to_instructionblock.iter() {
            if key.0 == pc {
                return (val.start_pc, val.end_pc);
            }
        }
        panic!("Could not find node for entry pc {pc}");
    }

    pub fn get_node_from_exit_pc(&self, pc: u16) -> (u16,u16) {
        for (key, val) in self.map_to_instructionblock.iter() {
            if key.1 == pc {
                return (val.start_pc, val.end_pc);
            }
        }
        panic!("Could not find node for exit pc {pc}");
    }

    pub fn step(&mut self, pc: u16, op: u8) -> Return {
        if self.last_node.is_none() {
            self.last_node = Some(self.get_node_from_entry_pc(pc));
        }
        match op {
            JUMP => self.jump(pc),

            JUMPI => {
                self.jumpi(pc);
            },
            JUMPDEST => self.jumpdest(pc),
            _ => {
                if self.get_node_from_pc(pc) != self.last_node.unwrap() {
                    self.attach_new_edge(pc)
                };
            },
        }
        Return::Continue
    }

    pub fn attach_new_edge(&mut self, entry_pc: u16) {
        let edge_to_use = self.default_edge_to_use();
        let entering_node = self.get_node_from_entry_pc(entry_pc);
        self.cfg_dag.add_edge(self.last_node.unwrap(), entering_node, edge_to_use);
        self.last_node = Some(entering_node);
    }

    pub fn default_edge_to_use(&mut self) -> Edges {
        if self.jumpi_edge.is_some() {
            let edge = self.jumpi_edge.unwrap();
            self.jumpi_edge = None;
            edge
        } else {
            Edges::Jump
        }
    }
    

    pub fn jump(&mut self, pc: u16) {
        // add in the jump node + prev edge
        if self.get_node_from_pc(pc) != self.last_node.unwrap() {
            self.attach_new_edge(pc)
        };
    }

    pub fn jumpi(&mut self, pc: u16) {
        if self.get_node_from_pc(pc) != self.last_node.unwrap() {
            self.attach_new_edge(pc)
        };
    }

    pub fn jumpdest(&mut self, pc: u16) {
        // add in the jumpdest node + take from the prev edge
        if self.get_node_from_pc(pc) != self.last_node.unwrap() {
            self.attach_new_edge(pc)
        };
    }


    pub fn cfg_dot_str_with_blocks(&mut self) -> String {
        /*
        digraph {
            node [shape=box, style=rounded, color="#565f89", fontcolor="#c0caf5", fontname="Helvetica"];
            edge [color="#565f89", fontcolor="#c0caf5", fontname="Helvetica"];
            bgcolor="#1a1b26";
            0 [ label = "pc0: PUSH1 0x80"]
            1 [ label = "pc2: JUMP" color = "red"]
            ...
        }
        */

        // have to use the petgraph module as the node indexes and edges are not the same as our weights
        let mut dot_str = Vec::new();
        let raw_start_str = r##"digraph G {
    node [shape=box, style="filled, rounded", color="#565f89", fontcolor="#c0caf5", fontname="Helvetica", fillcolor="#24283b"];
    edge [color="#414868", fontcolor="#c0caf5", fontname="Helvetica"];
    bgcolor="#1a1b26";"##;
        dot_str.push(raw_start_str.to_string());

        let nodes_and_edges_str = format!("{:?}", Dot::with_attr_getters(
            &self.cfg_dag,
            &[petgraph::dot::Config::GraphContentOnly, petgraph::dot::Config::NodeNoLabel, petgraph::dot::Config::EdgeNoLabel],
            &|_graph, edge_ref| {
                match edge_ref {
                    (_, _, &Edges::Jump) => {
                        "".to_string()
                    },
                    (_, _, &Edges::ConditionTrue) => {
                        format!("label = \"{:?}\" color = \"{}\"", edge_ref.weight(), TOKYO_NIGHT_COLORS.get("green").unwrap())
                    },
                    (_, _, &Edges::ConditionFalse) => {
                        format!("label = \"{:?}\" color = \"{}\"", edge_ref.weight(), TOKYO_NIGHT_COLORS.get("red").unwrap())
                    },
                    (_, _, &Edges::SymbolicJump) => {
                        format!("label = \"{:?}\" color = \"{}\", style=\"dotted, bold\"", edge_ref.weight(), TOKYO_NIGHT_COLORS.get("yellow").unwrap())
                    },
                }
            },
            &|_graph, (_id, node_ref)| {
                let mut node_str = String::new();
                let instruction_block = self.map_to_instructionblock.get(node_ref).unwrap();
                let color = instruction_block.node_color();
                match color {
                    Some(color) => {
                        node_str.push_str(&format!("label = \"{instruction_block:?}\" color = \"{color}\""));
                    },
                    None => {
                        node_str.push_str(&format!("label = \"{instruction_block:?}\""));
                    }
                }
                // if the node has no incoming edges, fill the node with deepred
                if instruction_block.start_pc == 0 {
                    node_str.push_str(" shape = invhouse");
                } else if self.cfg_dag.neighbors_directed(*node_ref, Incoming).count() == 0 {
                    node_str.push_str(&format!(" fillcolor = \"{}\"", TOKYO_NIGHT_COLORS.get("deepred").unwrap()));
                }
                node_str
            })
        );
        dot_str.push(nodes_and_edges_str);
        let raw_end_str = r#"}"#;
        dot_str.push(raw_end_str.to_string());
        dot_str.join("\n")
    }
}