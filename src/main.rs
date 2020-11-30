use anyhow::Result;
use std::env;
use std::io::prelude::*;
use std::io::{stderr, stdin, stdout};

mod common;

use common::lift_soln;

use env::Args;
fn main() {
    let days: Vec<Vec<Box<dyn Fn(String) -> Result<String>>>> = vec![vec![lift_soln(&day0)]];
    let out = stdout();
    let err = stderr();
    let mut args = env::args();
    args.next(); // Skip the program name
    if let Some((first, second)) = parse_usize(&mut args).zip(parse_usize(&mut args)) {
        if let Some(ref part) = days.get(first).and_then(|day| day.get(second)) {
            let mut buffer = Vec::<u8>::new();
            stdin().lock().read_to_end(&mut buffer).unwrap();
            let chars = String::from_utf8(buffer).unwrap();
            match part(chars) {
                Ok(o) => out.lock().write_all(&o.into_bytes()).unwrap(),
                Err(e) => err
                    .lock()
                    .write_all(&format!("error: {:?}", e).into_bytes())
                    .unwrap(),
            }
        } else {
            err.lock()
                .write_all(format!("unimplemented parts {} {}", first, second).as_bytes())
                .unwrap();
        }
    } else {
        err.lock()
            .write_all("usage: aoc2020 <day> <part>".as_bytes())
            .unwrap();
        std::process::exit(1);
    }
}

#[inline]
fn parse_usize(args: &mut Args) -> Option<usize> {
    args.next().and_then(|s| s.parse().ok())
}

fn day0(s: String) -> String {
    format!("echo..... {}", s)
}
