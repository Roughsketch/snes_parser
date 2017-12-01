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
    Reserved(u8, u8),
    Stack(u8),
    StackInterrupt(u8, u8),
    StackRelative(u8, u8),
    StackRelativeLong(u8, u16),
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
            Reserved(op, param)                 => write!(f, "{} {}", opcode_name(op), param),
            Stack(op)                           => write!(f, "{}", opcode_name(op)),
            StackInterrupt(op, param)           => write!(f, "{} #${:02X}", opcode_name(op), param),
            StackRelative(op, param)            => write!(f, "{} ${:02X}, s", opcode_name(op), param),
            StackRelativeLong(op, param)        => write!(f, "{} ${:04X}, s", opcode_name(op), param),
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

named!(absolute<Instruction>, 
    do_parse!(op: one_of!(
        [0x0C, 0x0D, 0x0E, 0x1C, 0x20, 0x2C, 0x2D, 0x2E, 
        0x4C, 0x4D, 0x4E, 0x6D, 0x6E, 0x8C, 0x8D, 0x8E,
        0x9C, 0xAC, 0xAD, 0xAE, 0xCC, 0xCD, 0xCE, 0xEC,
        0xED, 0xEE].as_ref())
    >> param: le_u16
    >> (Instruction::Absolute(op as u8, param))));

named!(absolute_indirect<Instruction>, 
    do_parse!(op: char!(0x6C as char)
    >> param: le_u16
    >> (Instruction::AbsoluteIndirect(op as u8, param))));

named!(absolute_indirect_x<Instruction>, 
    do_parse!(op: one_of!([0x7C, 0xFC].as_ref())
    >> param: le_u16
    >> (Instruction::AbsoluteIndirectX(op as u8, param))));

named!(absolute_indirect_long<Instruction>, 
    do_parse!(op: char!(0xDC as char)
    >> param: le_u16
    >> (Instruction::AbsoluteIndirectLong(op as u8, param))));

named!(absolute_x<Instruction>, 
    do_parse!(op: one_of!(
        [0x1D, 0x1E, 0x3C, 0x3D, 0x3E, 0x5D, 0x5E, 0x7D, 
        0x7E, 0x9D, 0x9E, 0xBC, 0xBD, 0xDD, 0xDE, 0xFD, 
        0xFE].as_ref())
    >> param: le_u16
    >> (Instruction::AbsoluteX(op as u8, param))));

named!(absolute_y<Instruction>, 
    do_parse!(op: one_of!(
        [0x19, 0x39, 0x59, 0x79, 0x99, 0xB9, 0xBE, 0xD9, 
        0xF9].as_ref())
    >> param: le_u16
    >> (Instruction::AbsoluteY(op as u8, param))));

named!(absolute_long<Instruction>, 
    do_parse!(op: one_of!(
        [0x0F, 0x22, 0x2F, 0x4F, 0x5C, 0x6F, 0x8F, 0xAF, 
        0xCF, 0xEF].as_ref())
    >> param: le_u24
    >> (Instruction::AbsoluteLong(op as u8, param))));

named!(absolute_long_x<Instruction>, 
    do_parse!(op: one_of!(
        [0x1F, 0x3F, 0x5F, 0x7F, 0x9F, 0xBF, 0xDF, 0xFF].as_ref())
    >> param: le_u24
    >> (Instruction::AbsoluteLongX(op as u8, param))));

named!(accumulator<Instruction>, 
    do_parse!(op: one_of!(
        [0x0A, 0x3A, 0x1A, 0x4A, 0x2A, 0x6A].as_ref())
        >> (Instruction::Accumulator(op as u8))));

named!(block<Instruction>, 
    do_parse!(op: one_of!([0x44, 0x54].as_ref())
        >> param1: le_u8
        >> param2: le_u8
        >> (Instruction::Block(op as u8, param1, param2))));

named!(direct<Instruction>, 
    do_parse!(op: one_of!(
        [0x04, 0x05, 0x06, 0x14, 0x24, 0x25, 0x26, 0x45, 
        0x46, 0x64, 0x65, 0x66, 0x84, 0x85, 0x86, 0xA4,
        0xA5, 0xA6, 0xC4, 0xC5, 0xC6, 0xE4, 0xE5, 0xE6].as_ref())
        >> param: le_u8
        >> (Instruction::Direct(op as u8, param))));

named!(direct_x<Instruction>, 
    do_parse!(op: one_of!(
        [0x15, 0x16, 0x34, 0x35, 0x36, 0x55, 0x56, 0x74,
        0x75, 0x76, 0x94, 0x95, 0xB4, 0xB5, 0xD5, 0xD6,
        0xF5, 0xF6,].as_ref())
        >> param: le_u8
        >> (Instruction::DirectX(op as u8, param))));

named!(direct_y<Instruction>, 
    do_parse!(op: one_of!([0xB6, 0x96].as_ref())
        >> param: le_u8
        >> (Instruction::DirectY(op as u8, param))));

named!(direct_indirect<Instruction>, 
    do_parse!(op: one_of!(
        [0x12, 0x32, 0x47, 0x52, 0x72, 0x92, 0xB2, 0xD2,0xF2].as_ref())
        >> param: le_u8
        >> (Instruction::DirectIndirect(op as u8, param))));

named!(direct_indirect_y<Instruction>, 
    do_parse!(op: one_of!(
        [0x11, 0x17, 0x31, 0x37, 0x51, 0x57, 0x71, 0x77,
        0x91, 0xB1, 0xB7, 0xD1, 0xD7, 0xF1].as_ref())
        >> param: le_u8
        >> (Instruction::DirectIndirectY(op as u8, param))));

named!(direct_indirect_x<Instruction>,
    do_parse!(op: one_of!(
        [0x01, 0x21, 0x41, 0x61, 0x81, 0xA1, 0xC1, 0xE1].as_ref())
    >> param: le_u8
    >> (Instruction::DirectIndirectX(op as u8, param))));

named!(direct_indirect_long<Instruction>,
    do_parse!(op: one_of!(
        [0x07, 0x27, 0x47, 0x67, 0x87, 0xA7, 0xC7, 0xE7].as_ref())
    >> param: le_u8
    >> (Instruction::DirectIndirectLong(op as u8, param))));

named!(direct_indirect_long_y<Instruction>,
    do_parse!(op: one_of!(
        [0x17, 0x37, 0x57, 0x77, 0x97, 0xB7, 0xD7, 0xF7].as_ref())
    >> param: le_u8
    >> (Instruction::DirectIndirectLongY(op as u8, param))));

named!(immediate<Instruction>, 
    do_parse!(op: one_of!(
        [0x09, 0x29, 0x49, 0x69, 0x89, 0xA0, 0xA2, 0xA9, 
        0xC0, 0xC2, 0xC9, 0xE0, 0xE2, 0xE9].as_ref())
    >> param: le_u8
    >> (Instruction::Immediate(op as u8, param))));

named!(implied<Instruction>, 
    do_parse!(op: one_of!(
        [0x18, 0x38, 0x58, 0x78, 0x88, 0x98, 0xA8, 0xB8, 
        0xC8, 0xD8, 0xE8, 0xF8, 0x0A, 0x1A, 0x2A, 0x3A, 
        0x4A, 0x6A, 0x8A, 0x9A, 0xAA, 0xBA, 0xCA, 0xEA, 
        0x1B, 0x3B, 0x5B, 0x7B, 0x9B, 0xBB, 0xCB, 0xDB, 
        0xEB, 0xFB].as_ref())
        >> (Instruction::Implied(op as u8))));

named!(relative<Instruction>,
    do_parse!(op: one_of!(
        [0x10, 0x30, 0x50, 0x70, 0x80, 0x90, 0xB0, 0xD0, 
        0xF0].as_ref())
    >> param: le_u8
    >> (Instruction::Relative(op as u8, param))));

named!(relative_long<Instruction>,
    do_parse!(op: char!(0x82 as char)
    >> param: le_u16
    >> (Instruction::RelativeLong(op as u8, param))));

named!(reserved<Instruction>,
    do_parse!(op: char!(0x42 as char)
    >> param: le_u8
    >> (Instruction::Reserved(op as u8, param))));

named!(stack<Instruction>,
    do_parse!(op: one_of!(
        [0x08, 0x0B, 0x28, 0x2B, 0x40, 0x48, 0x4B, 0x5A, 
        0x60, 0x68, 0x6B, 0x7A, 0x8B, 0xAB, 0xD4, 0xDA, 0xFA].as_ref())
        >> (Instruction::Stack(op as u8))));

named!(stack_interrupt<Instruction>,
    do_parse!(op: one_of!([0x00, 0x02].as_ref())
        >> param: le_u8
        >> (Instruction::StackInterrupt(op as u8, param))));

named!(stack_relative<Instruction>,
    do_parse!(op: one_of!([0x03, 0x23, 0x43, 0x63, 0x83, 0xA3, 0xC3, 0xE3].as_ref())
    >> param: le_u8
    >> (Instruction::StackRelative(op as u8, param))));

named!(stack_relative_long<Instruction>,
    do_parse!(op: one_of!([0x62, 0xF4].as_ref())
    >> param: le_u16
    >> (Instruction::StackRelativeLong(op as u8, param))));

named!(stack_relative_y<Instruction>,
    do_parse!(op: one_of!([0x13, 0x33, 0x53, 0x73, 0x93, 0xB3, 0xD3, 0xF3].as_ref())
    >> param: le_u8
    >> (Instruction::StackRelativeY(op as u8, param))));

named!(instruction<Instruction>,
    do_parse!(inst: alt!(
        absolute |
        absolute_indirect |
        absolute_indirect_long |
        absolute_indirect_x |
        absolute_x |
        absolute_y |
        absolute_long |
        absolute_long_x |
        accumulator |
        block |
        direct |
        direct_x |
        direct_y |
        direct_indirect |
        direct_indirect_x |
        direct_indirect_y |
        direct_indirect_long |
        direct_indirect_long_y |
        immediate |
        implied |
        relative |
        relative_long |
        reserved |
        stack |
        stack_interrupt |
        stack_relative |
        stack_relative_long |
        stack_relative_y)
        >> (inst)));

named!(pub parse_rom<Vec<Instruction>>, 
    fold_many1!(instruction, Vec::new(), 
        |mut vec: Vec<_>, inst| {
            vec.push(inst);
            vec
        }));
