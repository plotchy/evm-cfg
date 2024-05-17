use crate::cfg_gen::cfg_graph::*;
use revm::interpreter::opcode::*;
use std::{collections::BTreeSet, fmt::Debug};

pub mod cfg_graph;
pub mod dasm;
pub mod stack_solve;

pub const BLOCK_ENDERS_U8: [u8; 7] = [JUMP, JUMPI, STOP, RETURN, REVERT, INVALID, SELFDESTRUCT];

// Which opcodes will not convert our Concrete values into Symbolic values
pub const NON_DESTROYING_OPCODES: [u8; 1] = [
    // ADD, // may not be
    // MUL, // may not be
    // SUB, // may not be
    // i can probably add in div, mod, shl and such, but they require differentiating the 0 pos or 1 pos of the opcode
    // ie: a shl followed by a shr of the same shift size does not destroy the item, but i dont think this is common
    AND, // is not with ff's
        // OR,  // may not be
        // XOR,  // may not be
];

// Thank you JonBecker for this :)
#[derive(Clone, Debug, PartialEq)]
pub struct Opcode {
    pub name: String,
    pub mingas: u16,
    pub inputs: u16,
    pub outputs: u16,
}

// Returns the opcode for the given hexcode.
pub fn opcode(code: u8) -> Opcode {
    match code {
        0x00 => Opcode {
            name: String::from("STOP"),
            mingas: 0,
            inputs: 0,
            outputs: 0,
        },
        0x01 => Opcode {
            name: String::from("ADD"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x02 => Opcode {
            name: String::from("MUL"),
            mingas: 5,
            inputs: 2,
            outputs: 1,
        },
        0x03 => Opcode {
            name: String::from("SUB"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x04 => Opcode {
            name: String::from("DIV"),
            mingas: 5,
            inputs: 2,
            outputs: 1,
        },
        0x05 => Opcode {
            name: String::from("SDIV"),
            mingas: 5,
            inputs: 2,
            outputs: 1,
        },
        0x06 => Opcode {
            name: String::from("MOD"),
            mingas: 5,
            inputs: 2,
            outputs: 1,
        },
        0x07 => Opcode {
            name: String::from("SMOD"),
            mingas: 5,
            inputs: 2,
            outputs: 1,
        },
        0x08 => Opcode {
            name: String::from("ADDMOD"),
            mingas: 8,
            inputs: 3,
            outputs: 1,
        },
        0x09 => Opcode {
            name: String::from("MULMOD"),
            mingas: 8,
            inputs: 3,
            outputs: 1,
        },
        0x0a => Opcode {
            name: String::from("EXP"),
            mingas: 10,
            inputs: 2,
            outputs: 1,
        },
        0x0b => Opcode {
            name: String::from("SIGNEXTEND"),
            mingas: 5,
            inputs: 2,
            outputs: 1,
        },
        0x10 => Opcode {
            name: String::from("LT"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x11 => Opcode {
            name: String::from("GT"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x12 => Opcode {
            name: String::from("SLT"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x13 => Opcode {
            name: String::from("SGT"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x14 => Opcode {
            name: String::from("EQ"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x15 => Opcode {
            name: String::from("ISZERO"),
            mingas: 3,
            inputs: 1,
            outputs: 1,
        },
        0x16 => Opcode {
            name: String::from("AND"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x17 => Opcode {
            name: String::from("OR"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x18 => Opcode {
            name: String::from("XOR"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x19 => Opcode {
            name: String::from("NOT"),
            mingas: 3,
            inputs: 1,
            outputs: 1,
        },
        0x1a => Opcode {
            name: String::from("BYTE"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x1b => Opcode {
            name: String::from("SHL"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x1c => Opcode {
            name: String::from("SHR"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x1d => Opcode {
            name: String::from("SAR"),
            mingas: 3,
            inputs: 2,
            outputs: 1,
        },
        0x20 => Opcode {
            name: String::from("KECCAK256"),
            mingas: 30,
            inputs: 2,
            outputs: 1,
        },
        0x30 => Opcode {
            name: String::from("ADDRESS"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x31 => Opcode {
            name: String::from("BALANCE"),
            mingas: 100,
            inputs: 1,
            outputs: 1,
        },
        0x32 => Opcode {
            name: String::from("ORIGIN"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x33 => Opcode {
            name: String::from("CALLER"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x34 => Opcode {
            name: String::from("CALLVALUE"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x35 => Opcode {
            name: String::from("CALLDATALOAD"),
            mingas: 3,
            inputs: 1,
            outputs: 1,
        },
        0x36 => Opcode {
            name: String::from("CALLDATASIZE"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x37 => Opcode {
            name: String::from("CALLDATACOPY"),
            mingas: 3,
            inputs: 3,
            outputs: 0,
        },
        0x38 => Opcode {
            name: String::from("CODESIZE"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x39 => Opcode {
            name: String::from("CODECOPY"),
            mingas: 3,
            inputs: 3,
            outputs: 0,
        },
        0x3a => Opcode {
            name: String::from("GASPRICE"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x3b => Opcode {
            name: String::from("EXTCODESIZE"),
            mingas: 100,
            inputs: 1,
            outputs: 1,
        },
        0x3c => Opcode {
            name: String::from("EXTCODECOPY"),
            mingas: 100,
            inputs: 4,
            outputs: 0,
        },
        0x3d => Opcode {
            name: String::from("RETURNDATASIZE"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x3e => Opcode {
            name: String::from("RETURNDATACOPY"),
            mingas: 3,
            inputs: 3,
            outputs: 0,
        },
        0x3f => Opcode {
            name: String::from("EXTCODEHASH"),
            mingas: 100,
            inputs: 1,
            outputs: 1,
        },
        0x40 => Opcode {
            name: String::from("BLOCKHASH"),
            mingas: 20,
            inputs: 1,
            outputs: 1,
        },
        0x41 => Opcode {
            name: String::from("COINBASE"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x42 => Opcode {
            name: String::from("TIMESTAMP"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x43 => Opcode {
            name: String::from("NUMBER"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x44 => Opcode {
            name: String::from("DIFFICULTY"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x45 => Opcode {
            name: String::from("GASLIMIT"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x46 => Opcode {
            name: String::from("CHAINID"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x47 => Opcode {
            name: String::from("SELFBALANCE"),
            mingas: 5,
            inputs: 0,
            outputs: 1,
        },
        0x48 => Opcode {
            name: String::from("BASEFEE"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x50 => Opcode {
            name: String::from("POP"),
            mingas: 2,
            inputs: 1,
            outputs: 0,
        },
        0x51 => Opcode {
            name: String::from("MLOAD"),
            mingas: 3,
            inputs: 1,
            outputs: 1,
        },
        0x52 => Opcode {
            name: String::from("MSTORE"),
            mingas: 3,
            inputs: 2,
            outputs: 0,
        },
        0x53 => Opcode {
            name: String::from("MSTORE8"),
            mingas: 3,
            inputs: 2,
            outputs: 0,
        },
        0x54 => Opcode {
            name: String::from("SLOAD"),
            mingas: 100,
            inputs: 1,
            outputs: 1,
        },
        0x55 => Opcode {
            name: String::from("SSTORE"),
            mingas: 100,
            inputs: 2,
            outputs: 0,
        },
        0x56 => Opcode {
            name: String::from("JUMP"),
            mingas: 8,
            inputs: 1,
            outputs: 0,
        },
        0x57 => Opcode {
            name: String::from("JUMPI"),
            mingas: 10,
            inputs: 2,
            outputs: 0,
        },
        0x58 => Opcode {
            name: String::from("PC"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x59 => Opcode {
            name: String::from("MSIZE"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x5a => Opcode {
            name: String::from("GAS"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x5b => Opcode {
            name: String::from("JUMPDEST"),
            mingas: 1,
            inputs: 0,
            outputs: 0,
        },
        0x5c => Opcode {
            name: String::from("TLOAD"),
            mingas: 100,
            inputs: 1,
            outputs: 1,
        },
        0x5d => Opcode {
            name: String::from("TSTORE"),
            mingas: 100,
            inputs: 2,
            outputs: 0,
        },
        0x5f => Opcode {
            name: String::from("PUSH0"),
            mingas: 2,
            inputs: 0,
            outputs: 1,
        },
        0x60 => Opcode {
            name: String::from("PUSH1"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x61 => Opcode {
            name: String::from("PUSH2"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x62 => Opcode {
            name: String::from("PUSH3"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x63 => Opcode {
            name: String::from("PUSH4"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x64 => Opcode {
            name: String::from("PUSH5"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x65 => Opcode {
            name: String::from("PUSH6"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x66 => Opcode {
            name: String::from("PUSH7"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x67 => Opcode {
            name: String::from("PUSH8"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x68 => Opcode {
            name: String::from("PUSH9"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x69 => Opcode {
            name: String::from("PUSH10"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x6a => Opcode {
            name: String::from("PUSH11"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x6b => Opcode {
            name: String::from("PUSH12"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x6c => Opcode {
            name: String::from("PUSH13"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x6d => Opcode {
            name: String::from("PUSH14"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x6e => Opcode {
            name: String::from("PUSH15"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x6f => Opcode {
            name: String::from("PUSH16"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x70 => Opcode {
            name: String::from("PUSH17"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x71 => Opcode {
            name: String::from("PUSH18"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x72 => Opcode {
            name: String::from("PUSH19"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x73 => Opcode {
            name: String::from("PUSH20"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x74 => Opcode {
            name: String::from("PUSH21"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x75 => Opcode {
            name: String::from("PUSH22"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x76 => Opcode {
            name: String::from("PUSH23"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x77 => Opcode {
            name: String::from("PUSH24"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x78 => Opcode {
            name: String::from("PUSH25"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x79 => Opcode {
            name: String::from("PUSH26"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x7a => Opcode {
            name: String::from("PUSH27"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x7b => Opcode {
            name: String::from("PUSH28"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x7c => Opcode {
            name: String::from("PUSH29"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x7d => Opcode {
            name: String::from("PUSH30"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x7e => Opcode {
            name: String::from("PUSH31"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x7f => Opcode {
            name: String::from("PUSH32"),
            mingas: 3,
            inputs: 0,
            outputs: 1,
        },
        0x80 => Opcode {
            name: String::from("DUP1"),
            mingas: 3,
            inputs: 1,
            outputs: 2,
        },
        0x81 => Opcode {
            name: String::from("DUP2"),
            mingas: 3,
            inputs: 2,
            outputs: 3,
        },
        0x82 => Opcode {
            name: String::from("DUP3"),
            mingas: 3,
            inputs: 3,
            outputs: 4,
        },
        0x83 => Opcode {
            name: String::from("DUP4"),
            mingas: 3,
            inputs: 4,
            outputs: 5,
        },
        0x84 => Opcode {
            name: String::from("DUP5"),
            mingas: 3,
            inputs: 5,
            outputs: 6,
        },
        0x85 => Opcode {
            name: String::from("DUP6"),
            mingas: 3,
            inputs: 6,
            outputs: 7,
        },
        0x86 => Opcode {
            name: String::from("DUP7"),
            mingas: 3,
            inputs: 7,
            outputs: 8,
        },
        0x87 => Opcode {
            name: String::from("DUP8"),
            mingas: 3,
            inputs: 8,
            outputs: 9,
        },
        0x88 => Opcode {
            name: String::from("DUP9"),
            mingas: 3,
            inputs: 9,
            outputs: 10,
        },
        0x89 => Opcode {
            name: String::from("DUP10"),
            mingas: 3,
            inputs: 10,
            outputs: 11,
        },
        0x8a => Opcode {
            name: String::from("DUP11"),
            mingas: 3,
            inputs: 11,
            outputs: 12,
        },
        0x8b => Opcode {
            name: String::from("DUP12"),
            mingas: 3,
            inputs: 12,
            outputs: 13,
        },
        0x8c => Opcode {
            name: String::from("DUP13"),
            mingas: 3,
            inputs: 13,
            outputs: 14,
        },
        0x8d => Opcode {
            name: String::from("DUP14"),
            mingas: 3,
            inputs: 14,
            outputs: 15,
        },
        0x8e => Opcode {
            name: String::from("DUP15"),
            mingas: 3,
            inputs: 15,
            outputs: 16,
        },
        0x8f => Opcode {
            name: String::from("DUP16"),
            mingas: 3,
            inputs: 16,
            outputs: 17,
        },
        0x90 => Opcode {
            name: String::from("SWAP1"),
            mingas: 3,
            inputs: 2,
            outputs: 2,
        },
        0x91 => Opcode {
            name: String::from("SWAP2"),
            mingas: 3,
            inputs: 3,
            outputs: 3,
        },
        0x92 => Opcode {
            name: String::from("SWAP3"),
            mingas: 3,
            inputs: 4,
            outputs: 4,
        },
        0x93 => Opcode {
            name: String::from("SWAP4"),
            mingas: 3,
            inputs: 5,
            outputs: 5,
        },
        0x94 => Opcode {
            name: String::from("SWAP5"),
            mingas: 3,
            inputs: 6,
            outputs: 6,
        },
        0x95 => Opcode {
            name: String::from("SWAP6"),
            mingas: 3,
            inputs: 7,
            outputs: 7,
        },
        0x96 => Opcode {
            name: String::from("SWAP7"),
            mingas: 3,
            inputs: 8,
            outputs: 8,
        },
        0x97 => Opcode {
            name: String::from("SWAP8"),
            mingas: 3,
            inputs: 9,
            outputs: 9,
        },
        0x98 => Opcode {
            name: String::from("SWAP9"),
            mingas: 3,
            inputs: 10,
            outputs: 10,
        },
        0x99 => Opcode {
            name: String::from("SWAP10"),
            mingas: 3,
            inputs: 11,
            outputs: 11,
        },
        0x9a => Opcode {
            name: String::from("SWAP11"),
            mingas: 3,
            inputs: 12,
            outputs: 12,
        },
        0x9b => Opcode {
            name: String::from("SWAP12"),
            mingas: 3,
            inputs: 13,
            outputs: 13,
        },
        0x9c => Opcode {
            name: String::from("SWAP13"),
            mingas: 3,
            inputs: 14,
            outputs: 14,
        },
        0x9d => Opcode {
            name: String::from("SWAP14"),
            mingas: 3,
            inputs: 15,
            outputs: 15,
        },
        0x9e => Opcode {
            name: String::from("SWAP15"),
            mingas: 3,
            inputs: 16,
            outputs: 16,
        },
        0x9f => Opcode {
            name: String::from("SWAP16"),
            mingas: 3,
            inputs: 17,
            outputs: 17,
        },
        0xa0 => Opcode {
            name: String::from("LOG0"),
            mingas: 375,
            inputs: 2,
            outputs: 0,
        },
        0xa1 => Opcode {
            name: String::from("LOG1"),
            mingas: 750,
            inputs: 3,
            outputs: 0,
        },
        0xa2 => Opcode {
            name: String::from("LOG2"),
            mingas: 1125,
            inputs: 4,
            outputs: 0,
        },
        0xa3 => Opcode {
            name: String::from("LOG3"),
            mingas: 1500,
            inputs: 5,
            outputs: 0,
        },
        0xa4 => Opcode {
            name: String::from("LOG4"),
            mingas: 1875,
            inputs: 6,
            outputs: 0,
        },
        0xf0 => Opcode {
            name: String::from("CREATE"),
            mingas: 32000,
            inputs: 3,
            outputs: 1,
        },
        0xf1 => Opcode {
            name: String::from("CALL"),
            mingas: 100,
            inputs: 7,
            outputs: 1,
        },
        0xf2 => Opcode {
            name: String::from("CALLCODE"),
            mingas: 100,
            inputs: 7,
            outputs: 1,
        },
        0xf3 => Opcode {
            name: String::from("RETURN"),
            mingas: 0,
            inputs: 2,
            outputs: 0,
        },
        0xf4 => Opcode {
            name: String::from("DELEGATECALL"),
            mingas: 100,
            inputs: 6,
            outputs: 1,
        },
        0xf5 => Opcode {
            name: String::from("CREATE2"),
            mingas: 32000,
            inputs: 4,
            outputs: 1,
        },
        0xfa => Opcode {
            name: String::from("STATICCALL"),
            mingas: 100,
            inputs: 6,
            outputs: 1,
        },
        0xfd => Opcode {
            name: String::from("REVERT"),
            mingas: 0,
            inputs: 2,
            outputs: 0,
        },
        0xfe => Opcode {
            name: String::from("INVALID"),
            mingas: 0,
            inputs: 0,
            outputs: 0,
        },
        0xff => Opcode {
            name: String::from("SELFDESTRUCT"),
            mingas: 5000,
            inputs: 1,
            outputs: 0,
        },
        _ => Opcode {
            name: String::from("unknown"),
            mingas: 0,
            inputs: 0,
            outputs: 0,
        },
    }
}

fn get_u16_from_u8_slice(push_val: &[u8]) -> u16 {
    (*push_val.first().unwrap_or(&0) as u16) << 8 | (*push_val.get(1).unwrap_or(&0) as u16)
}

fn format_pc(pc: u16) -> String {
    let pad_width = if pc <= u8::MAX as u16 { 2 } else { 4 };
    format!("{pc:0>pad_width$x}")
}
