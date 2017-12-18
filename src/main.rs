#[macro_use]
extern crate nom;
#[macro_use]
extern crate lazy_static;

use nom::IResult::Done;

mod asm_65c816;
use asm_65c816::*;

fn main() {
    let input = include_bytes!("../test/smw.sfc");
    let sl = &input[..];

    match parse_rom(&sl[..0x8000]) {
        Done(_, inst) => {
            for i in inst {
                println!("{}", i);
            }
        }
        err => println!("Parse error: {:?}", err),
    }
}
