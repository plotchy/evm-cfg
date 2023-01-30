use itertools::Itertools;
use ethers::types::U256;
use revm::opcode::*;
use std::{collections::{HashMap, HashSet, VecDeque}, hash::Hash, fmt::Debug};
use crate::cfg_gen::*;
use fnv::FnvBuildHasher;

#[derive(Clone, Default)]
pub struct InstructionBlock {
    pub start_pc: u16,
    pub end_pc: u16,
    pub ops: Vec<(u16, u8, Option<U256>)>,
    pub indirect_jump: Option<u16>,
    pub push_vals: Vec<(U256, Option<BTreeSet<u16>>)>,
    pub stack_info: StackInfo,
}

#[derive(Clone, Default, Eq, PartialEq,)]
pub struct StackInfo {
    pub min_stack_size_required_for_entry: u16, // essentially the largest key within map_stack_entry_pos_to_stack_usage_pos
    pub stack_entry_pos_to_op_usage: HashMap<u16, HashSet<(u16, OpWithPos), FnvBuildHasher>, FnvBuildHasher>, // stack_pos: (pc, OpWithPos)
    pub stack_entry_pos_to_stack_exit_pos: HashMap<u16, HashSet<u16, FnvBuildHasher>, FnvBuildHasher>, // {entry_pos: [exit_pos1, exit_pos2, ...]}
    pub stack_size_delta: i16, // how much the stack size changes from entry to exit
    pub push_used_for_jump: Option<u16>, // if a push is used for a jump, this is the value of the push
}

impl Debug for StackInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        s.push_str(&format!("\nStack size req: {}, sizeΔ: {}\n", self.min_stack_size_required_for_entry, self.stack_size_delta));

        if !self.stack_entry_pos_to_op_usage.is_empty() {
            s.push_str("Entry->Op usage:\n");
            for (entry_pos, set) in self.stack_entry_pos_to_op_usage.iter().sorted_by(|a, b| a.0.cmp(b.0)) {
                for (pc, (op, pos)) in set.iter().sorted_by(|a, b| a.0.cmp(&b.0)) {
            
                    s.push_str(&format!("\t{}->{}:{}:{}\n", entry_pos, pc, OPCODE_JUMPMAP[*op as usize].unwrap_or("INVALID"), pos));
                }
            }
        }
        if !self.stack_entry_pos_to_stack_exit_pos.is_empty() {
            s.push_str("Entry->Exit:\n");
            for (entry_pos, exit_pos) in self.stack_entry_pos_to_stack_exit_pos.iter().sorted_by(|a, b| a.0.cmp(b.0))  {
                s.push_str(&format!("\t{entry_pos}->"));
                // convert exit_pos set to a vec of strings

                let mut comma_count = exit_pos.len() - 1;
                for val in exit_pos.iter().sorted_by(|a, b| a.cmp(b)) {
                    if *val > 1024 {
                        if comma_count > 0 {
                            s.push_str("😵, ");
                            comma_count -= 1;
                        } else {
                            s.push('😵');
                        }
                    } else if comma_count > 0 {
                        s.push_str(&format!("{val:?}, "));
                        comma_count -= 1;
                    } else {
                        s.push_str(&format!("{val:?}"));
                    }
                }
                s.push('\n');
            }
        }
        write!(f, "{s}")
    }
}

impl StackInfo {
    pub fn get_entry_stack_usage_by_pc(&self, pc: u16) -> Vec<(u8, u8, u16)> {
        let mut op_usage_to_entry = Vec::new();
        for (entry_pos, set) in self.stack_entry_pos_to_op_usage.iter() {
            for (op_pc, (op, pos)) in set.iter() {
                if *op_pc == pc {
                    op_usage_to_entry.push((*op, *pos, *entry_pos));
                }
            }
        }
        op_usage_to_entry
    }

    pub fn add_push_used_for_jump(&mut self, val: u16) {
        self.push_used_for_jump = Some(val);
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum StackElement {
    Entry(Vec<u16>),
    Generated(u16, OpWithPos), // (pc, (op, added_pos))
}

impl Debug for StackElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackElement::Entry(pcs) => {
                let mut pc_chain = String::new();
                for pc in pcs {
                    pc_chain.push_str(&format!("{pc} "));
                }
                write!(f, "{pc_chain}")
        },
            StackElement::Generated(pc, (op, pos)) => write!(f, "{}:{}:{}", pc, OPCODE_JUMPMAP[*op as usize].unwrap_or("INVALID"), pos),
        }
    }
}


pub type OpWithPos = (u8, u8);

impl Debug for InstructionBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        // start the string by doing a typical "pc14: PUSH2 0x16"
        for (pc, op, push_val) in &self.ops {
            let op_str = OPCODE_JUMPMAP[*op as usize].unwrap_or("INVALID");
            match push_val {
                Some(val) => s.push_str(&format!("pc{pc}: {op_str} {val}\n")),
                None => s.push_str(&format!("pc{pc}: {op_str}\n")),
            };
        }

        // then add the extra information
        if self.indirect_jump.is_some() {
            s.push_str("Indirect!\n");
        }

        s.push_str(&format!("{:?}", self.stack_info));

        write!(f, "{s}")
    }
}

impl InstructionBlock {
    pub fn new(start_pc: u16) -> Self {
        Self {
            start_pc,
            end_pc: 0,
            ops: Vec::new(),
            indirect_jump: None,
            push_vals: Vec::new(),
            stack_info: StackInfo::default(),
        }
    }

    pub fn add_instruction(&mut self, pc: u16, op: u8, push_val: Option<U256>) {
        self.ops.push((pc, op, push_val));
        if let Some(push_val) = push_val {
            self.push_vals.push((push_val, None));
        }
    }

    pub fn add_indirect_jump(&mut self, pc: u16) {
        self.indirect_jump = Some(pc);
    }

    pub fn add_push_val_stack_loc_on_exit(&mut self, val: U256, pos: u16) {
        for (push_val, stack_pos) in self.push_vals.iter_mut() {
            if *push_val == val {
                // insert blank set if none, then insert pos
                match stack_pos {
                    Some(stack_pos) => {
                        stack_pos.insert(pos);
                    }
                    None => {
                        *stack_pos = Some(BTreeSet::new());
                        stack_pos.as_mut().unwrap().insert(pos);
                    }
                }
            }
        }
    }

    pub fn end_block(&mut self, end_pc: u16, blocks: &mut Vec<InstructionBlock>) -> InstructionBlock {
        self.end_pc = end_pc;
        blocks.push(self.clone());
        InstructionBlock::new(end_pc + 1)
    } 

    pub fn node_color(&self) -> Option<String> {
        for (_pc, op, _push_val) in &self.ops {
            let op_str = OPCODE_JUMPMAP[*op as usize].unwrap_or("INVALID");
            if ["REVERT", "INVALID"].contains(&op_str) {
                return Some("red".to_string());
            } else if ["RETURN", "STOP"].contains(&op_str) {
                return Some("darkblue".to_string());
            } else if ["SELFDESTRUCT"].contains(&op_str) {
                return Some("gold".to_string());
            } else if ["JUMP", "JUMPI"].contains(&op_str) && self.indirect_jump.is_some() {
                return Some("teal".to_string());
            }
            // symbolic jump as teal
        }
        None
    }

    // analyze the stack info in an entry-agnostic way
    pub fn analyze_stack_info(&mut self) {
        assert!(self.stack_info == StackInfo::default(), "stack info should be empty");

        let mut min_stack_size_required_for_entry: u16 = 0;
        let mut stack_entries_touched: Vec<StackElement> = Vec::new();

        let mut stack_entry_pos_to_op_usage: HashMap<u16, HashSet<(u16, OpWithPos), FnvBuildHasher>, FnvBuildHasher> = HashMap::with_hasher(FnvBuildHasher::default());
        let mut stack_entry_pos_to_stack_exit_pos: HashMap<u16, HashSet<u16, FnvBuildHasher>, FnvBuildHasher> = HashMap::with_hasher(FnvBuildHasher::default());
        let mut push_used_for_jump = None;

        const REASONABLE_STARTING_STACK_SIZE: usize = 32; // this is just a 'default' size to start a vector with. this can error if a block has, say, N+1 pops in a row
        let mut stack: VecDeque<StackElement> = VecDeque::with_capacity(REASONABLE_STARTING_STACK_SIZE*2);
        for i in 0..REASONABLE_STARTING_STACK_SIZE as u16 {
            stack.push_back(StackElement::Entry(vec![i]));
        }

        // iterate over the ops
        for (pc, op, _push_val) in &self.ops {
            let pc = *pc;
            let op = *op;
            let opcode_info = opcode(op);
            let op_inputs = opcode_info.inputs as u8;
            let op_outputs = opcode_info.outputs as u8;

            match op {
                STOP => { /* do nothing */ },
                ADD => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                MUL => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SUB => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                DIV => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SDIV => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                MOD => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SMOD => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                ADDMOD => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                MULMOD => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                EXP => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SIGNEXTEND => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                LT => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                GT => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SLT => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SGT => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                EQ => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                ISZERO => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                AND => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                OR => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                XOR => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                NOT => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                BYTE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SHL => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SHR => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SAR => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SHA3 => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                ADDRESS => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                BALANCE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                ORIGIN => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                CALLER => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                CALLVALUE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                CALLDATALOAD => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                CALLDATASIZE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                CALLDATACOPY => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                CODESIZE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                CODECOPY => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                GASPRICE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                EXTCODESIZE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                EXTCODECOPY => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                RETURNDATASIZE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                RETURNDATACOPY => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                EXTCODEHASH => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                BLOCKHASH => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                COINBASE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                NUMBER => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                DIFFICULTY => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                GASLIMIT => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                CHAINID => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SELFBALANCE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                BASEFEE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                POP =>  InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                MLOAD => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                MSTORE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                MSTORE8 => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SLOAD => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                SSTORE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                JUMP => {
                    // iterate over the stack and check what the jump destination is
                    let elem = stack.front().expect("no jump destination found");
                    if let StackElement::Generated(pc, (op, _pos)) = elem {
                        if Vec::from_iter(0x60..=0x7F).contains(op)  {
                            // we know a push statement exited on the stack
                            // now we need to get the value that was pushed by checking pc against self.ops
                            let val = self.ops.iter().find(|(pc_instr, op_instr, _push_val)| {
                                pc_instr == pc && op_instr == op
                            }).map(|(_, _, push_val)| {
                                push_val.expect("no push val found for push statement {pc} {op}")
                            }).expect("no push statement found for push statement {pc} {op}");
                            let val_u16 = val.try_into().expect("push val is not u16");
                            push_used_for_jump = Some(val_u16);
                            // and set indirect jump as false in case this was detected as one earlier
                            self.indirect_jump = None;
                        }
                    }
                    InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched);
                },
                JUMPI => {
                    // iterate over the stack and check what the jump destination is
                    let elem = stack.front().expect("no jump destination found");
                    if let StackElement::Generated(pc, (op, _pos)) = elem {
                        if Vec::from_iter(0x60..=0x7F).contains(op)  {
                            // we know a push statement exited on the stack
                            // now we need to get the value that was pushed by checking pc against self.ops
                            let val = self.ops.iter().find(|(pc_instr, op_instr, _push_val)| {
                                pc_instr == pc && op_instr == op
                            }).map(|(_, _, push_val)| {
                                push_val.expect("no push val found for push statement {pc} {op}")
                            }).expect("no push statement found for push statement {pc} {op}");
                            let val_u16 = val.try_into().expect("push val is not u16");
                            push_used_for_jump = Some(val_u16);
                            // and set indirect jump as false in case this was detected as one earlier
                            self.indirect_jump = None;
                        }
                    }
                    InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched);
                },
                PC => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                MSIZE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                GAS => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                JUMPDEST => { /* do nothing */ },
                0x60..=0x7F => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                0x80..=0x8f => {
                    // dup
                    let dup_loc = (op - 0x80) as usize;
                    let duplicant_elem = stack[dup_loc].clone();
                    if let StackElement::Entry(_) =  duplicant_elem {
                        stack_entries_touched.push(duplicant_elem.clone());
                    }
                    stack.push_front(duplicant_elem);
                }
                0x90..=0x9f => {
                    // swap
                    let swap_loc = (op - 0x8f) as usize;
                    let swap_idx = stack[swap_loc].clone();
                    let first_elem = stack[0].clone();
                    if let StackElement::Entry(_) = first_elem {
                        stack_entries_touched.push(first_elem); // always touches the top of the stack
                    }
                    if let StackElement::Entry(_) = swap_idx {
                        stack_entries_touched.push(swap_idx.clone());
                    }
                    stack[swap_loc] = stack[0].clone();
                    stack[0] = swap_idx;
                }
                LOG0 => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                LOG1 => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                LOG2 => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                LOG3 => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                LOG4 => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                CREATE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                CALL => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                CALLCODE => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                RETURN => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                DELEGATECALL => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                CREATE2 => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                STATICCALL => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                REVERT => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                INVALID => { /* do nothing */},
                SELFDESTRUCT => InstructionBlock::n_in_m_out(op_inputs, op_outputs, &mut stack, pc, op, &mut stack_entry_pos_to_op_usage, &mut stack_entries_touched),
                _ => {  /* do nothing */ },
            }
        }
        // println!("\npc: {:?}", self.start_pc);
        // println!("{:?}",&stack_entries_touched);
        // println!("stack before retain: {:?}", &stack);

        // Now analyze stack to determine where the entry values end up
        for (i, elem) in stack.iter().enumerate() {
            if let StackElement::Entry(vals) = elem {
                for val in vals {
                    if stack_entries_touched.iter().filter_map(|stack_entry|{
                        if let StackElement::Entry(vals) = stack_entry {
                            Some(vals)
                        } else {
                            None
                        }
                    }).flatten().contains(&val) {
                        // add the exit position to the set of exit positions for this val
                        // println!("adding exit position {} for entry position {}", i, val);
                        stack_entry_pos_to_stack_exit_pos.entry(*val).or_insert_with(||HashSet::with_hasher(FnvBuildHasher::default())).insert(i as u16);
                    }
                }
            }
        }

        // iterate over the stack and check if there are push statements that exited on the stack
        // if there are, then we need to add them with self.add_push_val_stack_loc
        for (i, elem) in stack.iter().enumerate() {
            match elem {
                StackElement::Generated(pc, (op, _pos)) if Vec::from_iter(0x60..=0x7F).contains(op) => {
                    // we know a push statement exited on the stack
                    // now we need to get the value that was pushed by checking pc against self.ops
                    let val = self.ops.iter().find(|(pc_instr, op_instr, _push_val)| {
                        pc_instr == pc && op_instr == op
                    }).map(|(_, _, push_val)| {
                        push_val.expect("no push val found for push statement {pc} {op}")
                    }).expect("no push statement found for push statement {pc} {op}");

                    self.add_push_val_stack_loc_on_exit(val, i as u16);
                },
                _ => { /* do nothing */ },
            }
        }


        let stack_size_delta = stack.len() as i16 - REASONABLE_STARTING_STACK_SIZE as i16;

        // find the largest value of StackElement::Entry that is in the stack_entries_touched vector
        stack_entries_touched.iter().for_each(|elem| {
            if let StackElement::Entry(vals) = elem {
                for val in vals {
                    if *val+1 > min_stack_size_required_for_entry {
                        min_stack_size_required_for_entry = *val+1;
                    }
                }
            }
        });

        // remove the stack items that are untouched by retaining elements that are present in the stack_entries_touched vector
        stack.retain(|elem| {
            match elem {
                StackElement::Entry(_) => {
                    // retain only if the entry has been touched
                    stack_entries_touched.contains(elem)
                },
                _ => true, // keep, as it is a generated stack element
            }
        });
        // println!("stack after retain: {:?}", &stack);
        // println!("");

        // Now we can iterate over the stack_entry_pos_to_stack_exit_pos map and remove keys that have one value that is the same as the (key+stack_size_delta)
        stack_entry_pos_to_stack_exit_pos.retain(|key, val| {
            !(val.len() == 1 && val.contains(&((*key as i16 + stack_size_delta) as u16)))
        });
        // println!("stack_entry_pos_to_stack_exit_pos after retain: {:?}", &stack_entry_pos_to_stack_exit_pos);


        // now check if there are missing entry positions from the (0..reasonable_starting_stack_size)
        // range. If there are, then we need to add them to the stack_entry_pos_to_stack_exit_pos map as a "dead" value
        for i in 0..REASONABLE_STARTING_STACK_SIZE {
            // look for entry position i within the stack vector
            let is_in_stack = stack.iter().filter_map(|elem| {
                match elem {
                    StackElement::Entry(vals) => {
                        Some(vals)
                    },
                    _ => {None},
                }
            }).flatten()
            .any(|elem| {
                *elem == i as u16
            });
            let has_been_touched = stack_entries_touched.iter().filter_map(|elem| {
                match elem {
                    StackElement::Entry(vals) => {
                        Some(vals)
                    },
                    _ => {None},
                }
            }).flatten().any(|elem| {
                *elem == i as u16
            });
            // println!("i: {i}, is_in_stack: {}, has_been_touched: {}", is_in_stack, has_been_touched);
            if !is_in_stack && has_been_touched {
                // this entry position is not present in the stack, so we need to add it at an impossible exit position
                stack_entry_pos_to_stack_exit_pos.entry(i as u16).or_insert_with(||HashSet::with_hasher(FnvBuildHasher::default())).insert(1025);
            }
        }

        

        // update the stack info
        self.stack_info = StackInfo {
            min_stack_size_required_for_entry,
            stack_entry_pos_to_op_usage,
            stack_entry_pos_to_stack_exit_pos,
            stack_size_delta, 
            push_used_for_jump
        };
    }

    pub fn n_in_m_out(
        n: u8, 
        m: u8, 
        stack: &mut VecDeque<StackElement>, 
        pc: u16, 
        op: u8, 
        stack_entry_pos_to_op_usage: &mut HashMap<u16, HashSet<(u16, (u8, u8)), FnvBuildHasher>, FnvBuildHasher>, 
        stack_entries_touched: &mut Vec<StackElement>, 
    ) {
        assert!(m <= 1); // not possible to have more than one output in evm


        let vec_popped = stack.drain(0..n as usize).collect::<Vec<_>>();
        // for each element used in the op, update map_stack_entry_pos_to_stack_usage_pos 
        for (_i, elem) in vec_popped.iter().enumerate() {
            // check if element is a StackElement::Entry enum
            if let StackElement::Entry(_entry) = &elem {
                // if so, update the map of {entry: (pc (op, pos_in_op))}
                stack_entries_touched.push(elem.clone());
            }
        }
        if NON_DESTROYING_OPCODES.contains(&op) {
            assert!(n == 2 && m == 1, "logic for non-destroying stack breaks for opcodes not using 2 inputs 1 output"); // NON_DESTROYING_OPCODES should only have 2 inputs and 1 output, if not, this logic breaks
            // rather than place a Generated element on the stack, place an Entry value that
            match vec_popped.into_iter().take(2).collect_tuple::<(_,_)>().unwrap() {
                (StackElement::Entry(mut entry), StackElement::Entry(entry2)) => {
                    for val in entry.clone() {
                        stack_entry_pos_to_op_usage.entry(val).or_insert_with(||HashSet::with_hasher(FnvBuildHasher::default())).insert((pc, (op, 0_u8)));
                    }
                    for val in entry2.clone() {
                        stack_entry_pos_to_op_usage.entry(val).or_insert_with(||HashSet::with_hasher(FnvBuildHasher::default())).insert((pc, (op, 1_u8)));
                    }
                    
                    entry.extend(entry2); // extends in place
                    stack.push_front(StackElement::Entry(entry));
   
                },
                (StackElement::Entry(entry), _) => {
                    stack.push_front(StackElement::Entry(entry.clone()));
                    for val in entry {
                        stack_entry_pos_to_op_usage.entry(val).or_insert_with(||HashSet::with_hasher(FnvBuildHasher::default())).insert((pc, (op, 0_u8)));
                    }
                },
                (_, StackElement::Entry(entry2)) => {
                    stack.push_front(StackElement::Entry(entry2.clone()));
                    for val in entry2 {
                        stack_entry_pos_to_op_usage.entry(val).or_insert_with(||HashSet::with_hasher(FnvBuildHasher::default())).insert((pc, (op, 1_u8)));
                    }
                },
                _ => { 
                    stack.push_front(StackElement::Generated(pc, (op, 0)));
                },
            }
        } else {
            (0..m).for_each(|i| {
                stack.push_front(StackElement::Generated(pc, (op, i)))
            });
            for (i, elem) in vec_popped.into_iter().enumerate() {
                if let StackElement::Entry(entry) = elem {
                    for val in entry {
                        stack_entry_pos_to_op_usage.entry(val).or_insert_with(||HashSet::with_hasher(FnvBuildHasher::default())).insert((pc, (op, i as u8)));
                    }
                }
            }
        }
    }
}



pub fn disassemble(bytecode: &Vec<u8>) -> Vec<InstructionBlock> {
    let mut pc: u16 = 0;
    let mut blocks: Vec<InstructionBlock> = Vec::new();
    // Iterate over the bytecode, disassembling each instruction.
    let mut block = InstructionBlock::new(0);
    let mut push_flag: i32 = 0;
    while (pc as usize) < bytecode.len(){

        let op = bytecode[pc as usize];
        let op_str = OPCODE_JUMPMAP[op as usize];
        match op_str {
            Some(name) => {
                if name.contains("PUSH") {
                    let byte_count_to_push: u16 = name.replace("PUSH", "").parse().unwrap();
                    let pushed_bytes = match  bytecode.get(pc as usize + 1..pc as usize + 1 + byte_count_to_push as usize) {
                        Some(bytes) => {
                            let bytes_str = bytes.iter().map(|b| format!("{b:02x}")).collect::<Vec<String>>().join("");
                            U256::from_str_radix(&bytes_str, 16).unwrap()
                        },
                        None => {
                            // OoB, this can actually happen in the metadata often but is useless
                            let pushed_bytes = U256::from(0x45); // what actually happens in the evm is the remaining OoB bytes are treated as zeros and appended
                            block.add_instruction(pc, op, Some(pushed_bytes));
                            break;
                        }
                    };
                    block.add_instruction(pc, op, Some(pushed_bytes));
                    pc += byte_count_to_push;
                    push_flag = 2;
                } else if name.contains("JUMPDEST") {
                    if !block.ops.is_empty() {
                        // this is only used if the metadata doesnt end with a block ender
                        block.end_block(pc - 1, &mut blocks); // we are starting a new block, so end the old one with the previous pc
                    }
                    block = InstructionBlock::new(pc);
                    block.add_instruction(pc, op, None);
                } else if name.contains("JUMP") {
                    block.add_instruction(pc, op, None);
                    if push_flag != 1 {
                        block.add_indirect_jump(pc);
                    }
                    block = block.end_block(pc, &mut blocks);
                } else if BLOCK_ENDERS_U8.contains(&op) {
                    block.add_instruction(pc, op, None);
                    block = block.end_block(pc, &mut blocks);
                } else {
                    block.add_instruction(pc, op, None);
                }
            },
            None => {
                //invalid
                //end block, but idk, not sure how to handle this. another new block may not start
                block.add_instruction(pc, op, None);
                block = block.end_block(pc, &mut blocks);
            }
        }
        push_flag -= 1;
        pc += 1;
    }

    if !block.ops.is_empty() {
        // this is only used if the metadata doesnt end with a block ender
        block.end_block(pc, &mut blocks);
    }
    blocks
}