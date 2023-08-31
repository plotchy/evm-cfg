use fnv::FnvBuildHasher;
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

use crate::cfg_gen::cfg_graph::*;
use crate::cfg_gen::dasm::*;
use crate::cfg_gen::*;

pub const MAX_STACK_SIZE: u16 = 1024;
pub const TRACKED_ITEM_COUNT: usize = 20;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EdgeSet {
    pub from_pc: u16,
    pub to_pc: u16,
    pub stack: EdgeStack,
}

impl EdgeSet {
    pub fn new(from_pc: u16, to_pc: u16, stack: EdgeStack) -> Self {
        Self {
            from_pc,
            to_pc,
            stack,
        }
    }
}
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EdgeStack {
    ///this attempts to be the most lightweight representation of the symbolic stack
    // bit vector of stack positions used. 0 means not used, 1 means used
    // only allows tracking stack items up to 128 depth, this is a limitation. stacked bit vecs could go to 1024, but 128 is a likely more than enough
    pub stack_pos: u128,
    pub stack_size: u16,

    // Im only tracking TRACKED_ITEM_COUNT amount as a way to not explode memory usage for our visiting set. This is a limitation
    // In practice, even fun-reversing-challenge has at most 3 tracked items on the stack at once
    pub stack_items: [u16; TRACKED_ITEM_COUNT],
}

impl EdgeStack {
    pub fn is_item_in_pos_set(&self, pos: u16) -> bool {
        let bit = 1u128 << pos;
        self.stack_pos & bit != 0
    }

    pub fn get_item_in_stack_pos_as_u16(&self, pos: u16) -> u16 {
        assert!(pos < MAX_STACK_SIZE, "pos asked for is too large for stack");
        let stack_pos_bits = self.stack_pos;

        // count how many bits are set before the pos we are looking for
        let mut count = 0;
        for i in 0..pos {
            if stack_pos_bits & (1u128 << i) != 0 {
                count += 1;
            }
        }
        assert!(
            count < TRACKED_ITEM_COUNT,
            "tracked item count is too large for stack_items bit vec"
        );

        // now we need to get the count'th bit in the stack_items as a limb
        self.stack_items[count]
    }

    pub fn stack_update_with_bit_ops(
        mut self,
        stack_entry_pos_to_stack_exit_pos: &HashMap<
            u16,
            HashSet<u16, FnvBuildHasher>,
            FnvBuildHasher,
        >,
        stack_entry_default_adjustment: i16,
        push_vals: &[(Vec<u8>, Option<BTreeSet<u16>>)],
        set_all_valid_jumpdests: &HashSet<u16, FnvBuildHasher>,
    ) -> Self {
        let mut stack_pos = self.stack_pos;

        // store our initially set bits as a u16 vec
        let mut initially_set_bits = vec![];
        for i in 0..128 {
            if stack_pos & (1u128 << i) != 0 {
                initially_set_bits.push(i);
            }
        }

        let mut stack_item_vec = self.stack_items.to_vec();
        let mut exit_pos_vec = vec![];

        // for each of the entry_pos keys in the map, we need to check if the bit is set in stack_pos
        for (stack_entry_pos, stack_exit_pos_set) in stack_entry_pos_to_stack_exit_pos.iter() {
            let bit = 1u128 << stack_entry_pos;
            if stack_pos & bit != 0 {
                // we found an entry_pos that is set and affected
                // bit is set, so we need to negate it, remove it from initially_set_bits, and push the value and locations we should set it to for later
                stack_pos &= !bit;
                let pos_index = initially_set_bits
                    .iter()
                    .position(|&x| x == *stack_entry_pos)
                    .expect("initially_set_bits does not contain stack_entry_pos");
                let stack_item = stack_item_vec.remove(pos_index);
                initially_set_bits.remove(pos_index);
                for stack_exit_pos in stack_exit_pos_set.iter() {
                    if *stack_exit_pos == 1025 {
                        // this is saying it should die, dont add it
                        continue;
                    }
                    if stack_exit_pos <= &127 {
                        // cant go over max stack size
                        exit_pos_vec.push((stack_item, *stack_exit_pos));
                    } else {
                        println!("stack_exit_pos: {stack_exit_pos}, stack_pos: {stack_pos}, stack_entry_default_adjustment: {stack_entry_default_adjustment}");
                        panic!("we really hit a stack_exit_pos > 127?")
                    }
                }
            }
        }

        // now we need to apply the default adjustment to any bits that are still in the initially_set_bits vec and push those to the exit_pos_vec
        for stack_entry_pos in initially_set_bits.iter() {
            let stack_exit_pos = (*stack_entry_pos as i16 + stack_entry_default_adjustment) as u16;
            if stack_exit_pos <= 127 {
                let stack_item = stack_item_vec.remove(0);
                exit_pos_vec.push((stack_item, stack_exit_pos));
            } else {
                // this is saying it should die, dont add it
                continue;
            }
        }
        // Now, we need to add any push_vals that are <= u16 values
        for (push_val, exit_pos) in push_vals {
            // check if the push_val is <= u16
            if push_val.len() <= 2 {
                let push_val: u16 = get_u16_from_u8_slice(push_val);

                // check if the push_val is a valid jump dest
                if set_all_valid_jumpdests.contains(&push_val) {
                    // check if the exit_pos is Some, if so, we need to add it to exit_pos_vec as (push_val, exit_pos)
                    if let Some(exit_pos) = exit_pos {
                        for exit_pos in exit_pos.iter() {
                            if *exit_pos <= 127 {
                                exit_pos_vec.push((push_val, *exit_pos));
                            } else {
                                // this is saying it should die, dont add it
                                continue;
                            }
                        }
                    }
                }
            }
        }
        // check if we are tracking too many items
        if exit_pos_vec.len() > TRACKED_ITEM_COUNT {
            println!(
                "exit_pos_vec.len() > TRACKED_ITEM_COUNT: {} > {}",
                exit_pos_vec.len(),
                TRACKED_ITEM_COUNT
            );
            panic!("exit_pos_vec.len() > TRACKED_ITEM_COUNT");
        }

        // now sort the exit_pos_vec by the stack_exit_pos
        exit_pos_vec.sort_by(|a, b| a.1.cmp(&b.1));
        // convert the exit_pos_vec to a stack_pos and stack_items
        let mut new_stack_pos = 0u128;
        let mut new_stack_items = [0; TRACKED_ITEM_COUNT];
        for (i, (stack_item, stack_exit_pos)) in exit_pos_vec
            .into_iter()
            .take(TRACKED_ITEM_COUNT)
            .enumerate()
        {
            new_stack_pos |= 1u128 << stack_exit_pos;
            new_stack_items[i] = stack_item;
        }

        self.stack_pos = new_stack_pos;
        self.stack_items = new_stack_items;
        self.stack_size = ((self.stack_size as i16) + stack_entry_default_adjustment) as u16;
        self
    }
}

pub fn symbolic_cycle(
    cfg_runner: &mut CFGRunner,
    set_all_valid_jumpdests: &HashSet<u16, FnvBuildHasher>,
    label_symbolic_jumps: bool,
) {
    let start_node = cfg_runner.get_node_from_entry_pc(0);

    let mut visited_set: HashSet<EdgeSet, FnvBuildHasher> =
        HashSet::with_capacity_and_hasher(2056, FnvBuildHasher::default());
    let mut unvisited_queue: VecDeque<(&InstructionBlock, EdgeStack)> =
        VecDeque::with_capacity(1024);

    let first_visit = (
        cfg_runner.map_to_instructionblock.get(&start_node).unwrap(),
        EdgeStack::default(),
    );
    unvisited_queue.push_front(first_visit);

    // println!("\nEntering symbolic cycle \n");
    while !unvisited_queue.is_empty() {
        let (current_block, current_stack) = unvisited_queue.pop_front().unwrap();
        let current_node = (current_block.start_pc, current_block.end_pc);
        // println!("exploring current_node {:?} with stack: {:?}", current_node, current_stack);

        // ensure stack size is not too small nor large after executing opcodes
        let new_stack_size =
            (current_stack.stack_size as i16) + current_block.stack_info.stack_size_delta;
        if new_stack_size < 0 {
            // stack would be too small, can't traverse this path
            continue;
        }
        let new_stack_size: u16 = new_stack_size.try_into().unwrap();
        if new_stack_size > MAX_STACK_SIZE {
            // stack would be too large, can't traverse this path
            continue;
        }

        // check if last opcode is a jump in this block (JUMPIs dont have indirect jumps??? according to EtherSolve in practice)
        let (last_pc, last_op, _push_val) = current_block.ops.last().unwrap();
        if *last_op == 0x56 {
            // find op usage of jump and iterate over the values it uses
            let jump_usage = current_block
                .stack_info
                .get_entry_stack_usage_by_pc(*last_pc);
            if jump_usage.is_empty() {
                // this is a jump to a value generated in the block.
                // This could be symbolically created within the block (sload), or a push (right above or prior in the block)

                // check if the stack_info analysis has a push value for this jump
                if let Some(_push_val_for_jump) = &current_block.stack_info.push_used_for_jump {
                    // there is a push used for this jump and we know our next node is already in the cfg
                    let mut next_node = (0, 0);
                    let outgoing_edge_refs = cfg_runner.cfg_dag.all_edges();
                    for edge_ref in outgoing_edge_refs {
                        if edge_ref.0 .0 == current_node.0 {
                            next_node = edge_ref.1;
                            break;
                        }
                    }
                    // before pushing new states to queue, execute opcodes
                    let new_stack = current_stack.stack_update_with_bit_ops(
                        &current_block.stack_info.stack_entry_pos_to_stack_exit_pos,
                        current_block.stack_info.stack_size_delta,
                        &current_block.push_vals,
                        set_all_valid_jumpdests,
                    );
                    // last opcode is a jump, can only go to next node
                    let new_edge_set = EdgeSet::new(current_node.1, next_node.0, new_stack);
                    // check if we have already visited this edge
                    if visited_set.contains(&new_edge_set) {
                        // if so, kill this traverser and continue
                        continue;
                    }
                    // add to visited_set and push new state to queue
                    visited_set.insert(new_edge_set);
                    let next_block = cfg_runner.map_to_instructionblock.get(&next_node).unwrap();
                    unvisited_queue.push_front((next_block, new_stack));
                } else {
                    // This is a symbolic jump created within the block
                    println!(
                        "Symbolic jump to location created within block at jump pc: {last_pc}"
                    );
                    if label_symbolic_jumps {
                        println!("\t- Labeling symbolic jumps is enabled, output may be unbearable. change variable in main.rs if youd like, skipping this jump");
                        // add all jumpdests as possible next nodes as long as the current stack size is >= the jumpdest's required stack size
                        let mut next_nodes = Vec::new();
                        for pc in set_all_valid_jumpdests {
                            let next_node = cfg_runner.get_node_from_entry_pc(*pc);
                            // check if the current stack size is >= the jumpdest's required stack size
                            let jumpdest_block =
                                cfg_runner.map_to_instructionblock.get(&next_node).unwrap();
                            if current_stack.stack_size
                                > jumpdest_block.stack_info.min_stack_size_required_for_entry
                            {
                                cfg_runner.cfg_dag.add_edge(
                                    current_node,
                                    next_node,
                                    Edges::SymbolicJump,
                                );
                                next_nodes.push(next_node);
                            }
                        }

                        // before pushing new states to queue, execute opcodes
                        let new_stack = current_stack.stack_update_with_bit_ops(
                            &current_block.stack_info.stack_entry_pos_to_stack_exit_pos,
                            current_block.stack_info.stack_size_delta,
                            &current_block.push_vals,
                            set_all_valid_jumpdests,
                        );

                        for next_node in next_nodes {
                            let new_edge_set = EdgeSet::new(current_node.1, next_node.0, new_stack);
                            // check if we have already visited this edge
                            if visited_set.contains(&new_edge_set) {
                                // if so, skip this next node and continue
                                continue;
                            }
                            // add to visited_set and push new state to queue
                            visited_set.insert(new_edge_set);
                            let _next_block =
                                cfg_runner.map_to_instructionblock.get(&next_node).unwrap();
                            // unvisited_queue.push_front((next_block, new_stack)); // executing at symbolic points gets super messy, as it treats all unknown values as symbolic
                        }
                    } else {
                        // not labeling symbolic jumps, just end the traverser
                        println!("\t- Labeling symbolic jumps is disabled, change variable in main.rs if youd like, skipping this jump");
                        continue;
                    }
                }
            } else {
                // this is a jump to one of the values that entered the block from the stack
                let (_op, _pos, entry_pos) = *jump_usage.first().unwrap();
                // this is the jump's location
                // check if the entry_pos is a set bit in our stack
                if current_stack.is_item_in_pos_set(entry_pos) {
                    // our stack has a pushed value at this position
                    let jump_dest = current_stack.get_item_in_stack_pos_as_u16(entry_pos);
                    let next_node = cfg_runner.get_node_from_entry_pc(jump_dest);
                    // add edge to cfg
                    cfg_runner
                        .cfg_dag
                        .add_edge(current_node, next_node, Edges::Jump);

                    // before pushing new states to queue, execute opcodes
                    let new_stack = current_stack.stack_update_with_bit_ops(
                        &current_block.stack_info.stack_entry_pos_to_stack_exit_pos,
                        current_block.stack_info.stack_size_delta,
                        &current_block.push_vals,
                        set_all_valid_jumpdests,
                    );

                    // last opcode is a jump, can only go to next node
                    let new_edge_set = EdgeSet::new(current_node.1, next_node.0, new_stack);
                    // check if we have already visited this edge
                    if visited_set.contains(&new_edge_set) {
                        // if so, kill this traverser and continue
                        continue;
                    }
                    // add to visited_set and push new state to queue
                    visited_set.insert(new_edge_set);
                    let next_block = cfg_runner.map_to_instructionblock.get(&next_node).unwrap();
                    unvisited_queue.push_front((next_block, new_stack));
                } else {
                    // we do not have a tracked push value for this entry, this is symbolic
                    // This is a symbolic jump from an entry position not tracked
                    println!("Stack Entry not tracked for this jump. Symbolic jump to stack loc at jump pc: {last_pc}");
                    if label_symbolic_jumps {
                        println!("\t- Labeling symbolic jumps is enabled, output may be unbearable. change variable in main.rs if youd like, skipping this jump");
                        // add all jumpdests as possible next nodes as long as the current stack size is >= the jumpdest's required stack size
                        let mut next_nodes = Vec::new();
                        for pc in set_all_valid_jumpdests {
                            let next_node = cfg_runner.get_node_from_entry_pc(*pc);
                            // check if the current stack size is >= the jumpdest's required stack size
                            let jumpdest_block =
                                cfg_runner.map_to_instructionblock.get(&next_node).unwrap();
                            if current_stack.stack_size
                                > jumpdest_block.stack_info.min_stack_size_required_for_entry
                            {
                                cfg_runner.cfg_dag.add_edge(
                                    current_node,
                                    next_node,
                                    Edges::SymbolicJump,
                                );
                                next_nodes.push(next_node);
                            }
                        }

                        // before pushing new states to queue, execute opcodes
                        let new_stack = current_stack.stack_update_with_bit_ops(
                            &current_block.stack_info.stack_entry_pos_to_stack_exit_pos,
                            current_block.stack_info.stack_size_delta,
                            &current_block.push_vals,
                            set_all_valid_jumpdests,
                        );

                        for next_node in next_nodes {
                            let new_edge_set = EdgeSet::new(current_node.1, next_node.0, new_stack);
                            // check if we have already visited this edge
                            if visited_set.contains(&new_edge_set) {
                                // if so, skip this next node and continue
                                continue;
                            }
                            // add to visited_set and push new state to queue
                            visited_set.insert(new_edge_set);
                            let _next_block =
                                cfg_runner.map_to_instructionblock.get(&next_node).unwrap();
                            // unvisited_queue.push_front((next_block, new_stack)); // executing at symbolic points gets super messy, as it treats all unknown values as symbolic
                        }
                    } else {
                        // not labeling symbolic jumps, just end the traverser
                        println!("\t- Labeling symbolic jumps is disabled, change variable in main.rs if youd like, skipping this jump");
                        continue;
                    }
                }
            }
        } else {
            // before pushing new states to queue, execute opcodes
            let new_stack = current_stack.stack_update_with_bit_ops(
                &current_block.stack_info.stack_entry_pos_to_stack_exit_pos,
                current_block.stack_info.stack_size_delta,
                &current_block.push_vals,
                set_all_valid_jumpdests,
            );

            // since last opcode is not a jump, we can continue to the next block with each of our successors
            let outgoing_edge_refs = cfg_runner.cfg_dag.all_edges();
            let mut outgoing_edges = Vec::with_capacity(10);
            for edge_ref in outgoing_edge_refs {
                if edge_ref.0 .0 == current_node.0 {
                    outgoing_edges.push(edge_ref.1);
                }
            }
            for next_node in outgoing_edges {
                // convert this edge to an edge_set
                let new_edge_set = EdgeSet::new(current_node.1, next_node.0, new_stack);
                // check if we have already visited this edge
                if visited_set.contains(&new_edge_set) {
                    // if so, continue to next successor
                    continue;
                }
                // add to visited_set and push new state to queue
                visited_set.insert(new_edge_set);
                let next_block = cfg_runner.map_to_instructionblock.get(&next_node).unwrap();
                // println!("Pushing new state to queue: {:?} -> {:?} -> {:?}", current_node, next_node, new_stack);
                unvisited_queue.push_front((next_block, new_stack));
            }
        }
    }
    // println!("Done building CFG");
}
