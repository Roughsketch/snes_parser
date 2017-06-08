#[macro_use]
extern crate nom;
#[macro_use]
extern crate lazy_static;

use nom::IResult::Done;

mod asm_65c816;
use asm_65c816::*;

fn main() {
    let input = include_bytes!("../test/test.sfc");
    let sl = &input[..];

    if let Done(_, inst) = parse_rom(sl) {
        for i in inst {
            println!("{}", i);
        }
    }
}
