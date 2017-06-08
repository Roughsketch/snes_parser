use nom::{le_u8, le_u16, le_u24};

use std::fmt;

#[derive(Debug)]
pub enum Instruction {
    Absolute(u8, u16),
    AbsoluteX(u8, u16),
    AbsoluteY(u8, u16),
    AbsoluteIndirect(u8, u16),
    AbsoluteIndirectX(u8, u16),
    AbsoluteIndirectLong(u8, u16),
    AbsoluteLong(u8, u32),
    AbsoluteLongX(u8, u32),
    Accumulator(u8),
    Block(u8, u8, u8),
    Direct(u8, u8),
    DirectX(u8, u8),
    DirectY(u8, u8),
    DirectIndirect(u8, u8),
    DirectIndirectX(u8, u8),
    DirectIndirectY(u8, u8),
    DirectIndirectLong(u8, u8),
    DirectIndirectLongY(u8, u8),
    Immediate(u8, u8),
    Implied(u8),
    Relative(u8, u8),
    RelativeLong(u8, u16),
    Stack(u8),
    StackInterrupt(u8, u8),
    StackRelative(u8, u8),
    StackRelativeY(u8, u8),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Instruction::*;

        match *self {
            Absolute(op, param)                 => write!(f, "{} ${:04X}", opcode_name(op), param),
            AbsoluteX(op, param)                => write!(f, "{} ${:04X}, x", opcode_name(op), param),
            AbsoluteY(op, param)                => write!(f, "{} ${:04X}, y", opcode_name(op), param),
            AbsoluteIndirect(op, param)         => write!(f, "{} (${:04X})", opcode_name(op), param),
            AbsoluteIndirectX(op, param)        => write!(f, "{} (${:04X}, x)", opcode_name(op), param),
            AbsoluteIndirectLong(op, param)     => write!(f, "{} [${:04X}]", opcode_name(op), param),
            AbsoluteLong(op, param)             => write!(f, "{} ${:06X}", opcode_name(op), param),
            AbsoluteLongX(op, param)            => write!(f, "{} ${:06X}, x", opcode_name(op), param),
            Accumulator(op)                     => write!(f, "{} A", opcode_name(op)),
            Block(op, param1, param2)           => write!(f, "{} ${:02X}, ${:02X}", opcode_name(op), param1, param2),
            Direct(op, param)                   => write!(f, "{} ${:02X}", opcode_name(op), param),
            DirectX(op, param)                  => write!(f, "{} ${:02X}, x", opcode_name(op), param),
            DirectY(op, param)                  => write!(f, "{} ${:02X}, y", opcode_name(op), param),
            DirectIndirect(op, param)           => write!(f, "{} (${:02X})", opcode_name(op), param),
            DirectIndirectX(op, param)          => write!(f, "{} (${:02X}, x)", opcode_name(op), param),
            DirectIndirectY(op, param)          => write!(f, "{} (${:02X}), y", opcode_name(op), param),
            DirectIndirectLong(op, param)       => write!(f, "{} [${:02X}]", opcode_name(op), param),
            DirectIndirectLongY(op, param)      => write!(f, "{} [${:02X}], y", opcode_name(op), param),
            Immediate(op, param)                => write!(f, "{} #{:02X}", opcode_name(op), param),
            Implied(op)                         => write!(f, "{}", opcode_name(op)),
            Relative(op, param)                 => write!(f, "{} ${:02X}", opcode_name(op), param),
            RelativeLong(op, param)             => write!(f, "{} ${:04X}", opcode_name(op), param),
            Stack(op)                           => write!(f, "{}", opcode_name(op)),
            StackInterrupt(op, param)           => write!(f, "{} #${:02X}", opcode_name(op), param),
            StackRelative(op, param)            => write!(f, "{} ${:02X}, s", opcode_name(op), param),
            StackRelativeY(op, param)           => write!(f, "{} (${:02X}, s), y", opcode_name(op), param),
        }
    }
}

fn opcode_name(op: u8) -> &'static str {
    lazy_static! {
        static ref OPNAMES: Vec<&'static str> = vec!
        [
            "BRK", "ORA", "COP", "ORA", "TSB", "ORA", "ASL", "ORA",
            "PHP", "ORA", "ASL", "PHD", "TSB", "ORA", "ASL", "ORA",
            "BPL", "ORA", "ORA", "ORA", "TRB", "ORA", "ASL", "ORA",
            "CLC", "ORA", "INC", "TCS", "TRB", "ORA", "ASL", "ORA",
            "JSR", "AND", "JSL", "AND", "BIT", "AND", "ROL", "AND",
            "PLP", "AND", "ROL", "PLD", "BIT", "AND", "ROL", "AND",
            "BMI", "AND", "AND", "AND", "BIT", "AND", "ROL", "AND",
            "SEC", "AND", "DEC", "TSC", "BIT", "AND", "ROL", "AND",
            "RTI", "EOR", "WDM", "EOR", "MVP", "EOR", "LSR", "EOR",
            "PHA", "EOR", "LSR", "PHK", "JMP", "EOR", "LSR", "EOR",
            "BVC", "EOR", "EOR", "EOR", "MVN", "EOR", "LSR", "EOR",
            "CLI", "EOR", "PHY", "TCD", "JMP", "EOR", "LSR", "EOR",
            "RTS", "ADC", "PER", "ADC", "STZ", "ADC", "ROR", "ADC",
            "PLA", "ADC", "ROR", "RTL", "JMP", "ADC", "ROR", "ADC",
            "BVS", "ADC", "ADC", "ADC", "STZ", "ADC", "ROR", "ADC",
            "SEI", "ADC", "PLY", "TDC", "JMP", "ADC", "ROR", "ADC",
            "BRA", "STA", "BRL", "STA", "STY", "STA", "STX", "STA",
            "DEY", "BIT", "TXA", "PHB", "STY", "STA", "STX", "STA",
            "BCC", "STA", "STA", "STA", "STY", "STA", "STX", "STA",
            "TYA", "STA", "TXS", "TXY", "STZ", "STA", "STZ", "STA",
            "LDY", "LDA", "LDX", "LDA", "LDY", "LDA", "LDX", "LDA",
            "TAY", "LDA", "TAX", "PLB", "LDY", "LDA", "LDX", "LDA",
            "BCS", "LDA", "LDA", "LDA", "LDY", "LDA", "LDX", "LDA",
            "CLV", "LDA", "TSX", "TYX", "LDY", "LDA", "LDX", "LDA",
            "CPY", "CMP", "REP", "CMP", "CPY", "CMP", "DEC", "CMP",
            "INY", "CMP", "DEX", "WAI", "CPY", "CMP", "DEC", "CMP",
            "BNE", "CMP", "CMP", "CMP", "PEI", "CMP", "DEC", "CMP",
            "CLD", "CMP", "PHX", "STP", "JML", "CMP", "DEC", "CMP",
            "CPX", "SBC", "SEP", "SBC", "CPX", "SBC", "INC", "SBC",
            "INX", "SBC", "NOP", "XBA", "CPX", "SBC", "INC", "SBC",
            "BEQ", "SBC", "SBC", "SBC", "PEA", "SBC", "INC", "SBC",
            "SED", "SBC", "PLX", "XCE", "JSR", "SBC", "INC", "SBC",
        ];
    }

    OPNAMES[op as usize]
}

macro_rules! opcode(
    ($i:expr, $op:expr) => (
        tag_bits!($i, u8, 8, $op);
    );
    ($i:expr, $f:expr) => (
        opcode!($i, call!($f));
    );
);

named!(absolute<Instruction>, 
    do_parse!(op: bits!(alt!(
        opcode!(0x0C) | opcode!(0x0D) | opcode!(0x0E) | opcode!(0x1C) | 
        opcode!(0x20) | opcode!(0x2C) | opcode!(0x2D) | opcode!(0x2E) | 
        opcode!(0x4C) | opcode!(0x4D) | opcode!(0x4E) | opcode!(0x6D) | 
        opcode!(0x6E) | opcode!(0x8D) | opcode!(0x8E) | opcode!(0x9C) | 
        opcode!(0xAC) | opcode!(0xAD) | opcode!(0xAE) | opcode!(0xCC) | 
        opcode!(0xCD) | opcode!(0xCE) | opcode!(0xEC) | opcode!(0xED) | 
        opcode!(0xEE)))
    >> param: le_u16
    >> (Instruction::Absolute(op, param))));

named!(absolute_x<Instruction>, 
    do_parse!(op: bits!(alt!(
        opcode!(0x1D) | opcode!(0x1E) | opcode!(0x3C) | opcode!(0x3D) | 
        opcode!(0x3E) | opcode!(0x5D) | opcode!(0x5E) | opcode!(0x7D) | 
        opcode!(0x7E) | opcode!(0x9D) | opcode!(0x9E) | opcode!(0xBC) | 
        opcode!(0xBD) | opcode!(0xDD) | opcode!(0xDE) | opcode!(0xFD) | 
        opcode!(0xFE)))
    >> param: le_u16
    >> (Instruction::AbsoluteX(op, param))));

named!(absolute_y<Instruction>, 
    do_parse!(op: bits!(alt!(
        opcode!(0x19) | opcode!(0x39) | opcode!(0x59) | opcode!(0x79) | 
        opcode!(0x99) | opcode!(0xB9) | opcode!(0xBE) | opcode!(0xD9) | 
        opcode!(0xF9)))
    >> param: le_u16
    >> (Instruction::AbsoluteY(op, param))));

named!(absolute_long<Instruction>, 
    do_parse!(op: bits!(alt!(
        opcode!(0x0F) | opcode!(0x22) | opcode!(0x2F) | opcode!(0x4F) | 
        opcode!(0x5C) | opcode!(0x6F) | opcode!(0x8F) | opcode!(0xAF) | 
        opcode!(0xCF) | opcode!(0xEF)))
    >> param: le_u24
    >> (Instruction::AbsoluteLong(op, param))));

named!(absolute_long_x<Instruction>, 
    do_parse!(op: bits!(alt!(
        opcode!(0x1F) | opcode!(0x3F) | opcode!(0x5F) | opcode!(0x7F) | 
        opcode!(0x9F) | opcode!(0xBF) | opcode!(0xDF) | opcode!(0xFF)))
    >> param: le_u24
    >> (Instruction::AbsoluteLongX(op, param))));

named!(accumulator<Instruction>, 
    do_parse!(op: bits!(alt!(
        opcode!(0x0A) | opcode!(0x3A) | opcode!(0x1A) | opcode!(0x4A) | 
        opcode!(0x2A) | opcode!(0x6A)))
        >> (Instruction::Accumulator(op))));

named!(direct<Instruction>, 
    do_parse!(op: bits!(alt!(
        opcode!(0x04) | opcode!(0x05) | opcode!(0x06) | opcode!(0x14) | 
        opcode!(0x24) | opcode!(0x25) | opcode!(0x26) | opcode!(0x45) | 
        opcode!(0x46) | opcode!(0x64) | opcode!(0x65) | opcode!(0x66) | 
        opcode!(0x85) | opcode!(0x86) | opcode!(0xA4) | opcode!(0xA5) | 
        opcode!(0xA6) | opcode!(0xC4) | opcode!(0xC5) | opcode!(0xC6) | 
        opcode!(0xE4) | opcode!(0xE5) | opcode!(0xE6)))
        >> param: le_u8
        >> (Instruction::Direct(op, param))));

named!(direct_indirect_x<Instruction>,
    do_parse!(op: bits!(alt!(
        opcode!(0x01) | opcode!(0x21) | opcode!(0x41) | opcode!(0x81) | 
        opcode!(0xA1) | opcode!(0xC1) | opcode!(0xE1)))
    >> param: le_u8
    >> (Instruction::DirectIndirectX(op, param))));

named!(immediate<Instruction>, 
    do_parse!(op: bits!(alt!(
        opcode!(0x09) | opcode!(0x29) | opcode!(0x49) | opcode!(0x69) | 
        opcode!(0x89) | opcode!(0xA0) | opcode!(0xA2) | opcode!(0xA9) | 
        opcode!(0xC0) | opcode!(0xC2) | opcode!(0xC9) | opcode!(0xE0) | 
        opcode!(0xE2) | opcode!(0xE9)))
    >> param: le_u8
    >> (Instruction::Immediate(op, param))));

named!(implied<Instruction>, 
    do_parse!(op: bits!(alt!(
        opcode!(0x18) | opcode!(0x38) | opcode!(0x58) | opcode!(0x78) | 
        opcode!(0x88) | opcode!(0x98) | opcode!(0xA8) | opcode!(0xB8) | 
        opcode!(0xC8) | opcode!(0xD8) | opcode!(0xE8) | opcode!(0xF8) | 
        opcode!(0x0A) | opcode!(0x1A) | opcode!(0x2A) | opcode!(0x3A) | 
        opcode!(0x4A) | opcode!(0x6A) | opcode!(0x8A) | opcode!(0x9A) | 
        opcode!(0xAA) | opcode!(0xBA) | opcode!(0xCA) | opcode!(0xEA) | 
        opcode!(0x1B) | opcode!(0x3B) | opcode!(0x5B) | opcode!(0x7B) | 
        opcode!(0x9B) | opcode!(0xBB) | opcode!(0xCB) | opcode!(0xDB) | 
        opcode!(0xEB) | opcode!(0xFB)))
        >> (Instruction::Implied(op))));

named!(relative<Instruction>,
    do_parse!(op: bits!(alt!(
        opcode!(0x10) | opcode!(0x30) | opcode!(0x50) | opcode!(0x70) | 
        opcode!(0x80) | opcode!(0x90) | opcode!(0xB0) | opcode!(0xD0) | 
        opcode!(0xF0)))
    >> param: le_u8
    >> (Instruction::Relative(op, param))));

named!(stack<Instruction>,
    do_parse!(op: bits!(alt!(
        opcode!(0x08) | opcode!(0x0B) | opcode!(0x28) | opcode!(0x2B) | 
        opcode!(0x40) | opcode!(0x48) | opcode!(0x4B) | opcode!(0x5A) | 
        opcode!(0x60) | opcode!(0x62) | opcode!(0x68) | opcode!(0x6B) | 
        opcode!(0x7A) | opcode!(0x8B) | opcode!(0xAB) | opcode!(0xD4) | 
        opcode!(0xDA) | opcode!(0xF4) | opcode!(0xFA)))
        >> (Instruction::Stack(op))));

named!(stack_interrupt<Instruction>,
    do_parse!(op: bits!(alt!(
        opcode!(0x00) | opcode!(0x02)))
        >> param: le_u8
        >> (Instruction::StackInterrupt(op, param))));

named!(stack_relative<Instruction>,
    do_parse!(op: bits!(alt!(
        opcode!(0x03) | opcode!(0x23) | opcode!(0x43) | opcode!(0x83) | 
        opcode!(0xA3) | opcode!(0xC3) | opcode!(0xE3)))
    >> param: le_u8
    >> (Instruction::StackRelative(op, param))));

named!(instruction<Instruction>,
    do_parse!(inst: alt!(
        absolute | absolute_x | absolute_y | absolute_long | absolute_long_x |
        accumulator | direct | direct_indirect_x | immediate | implied |
        relative | stack | stack_interrupt | stack_relative)
        >> (inst)));

named!(pub parse_rom<Vec<Instruction>>, 
    fold_many1!(instruction, Vec::new(), 
        |mut vec: Vec<_>, inst| {
            vec.push(inst);
            vec
        }));
