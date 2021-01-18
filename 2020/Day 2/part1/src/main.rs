#[macro_use] extern crate lazy_static;
extern crate regex;

use std::env;
use std::fs::File;
use std::io::{self, prelude::*, BufReader};
use regex::Regex;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    assert_eq!(2, args.len());

    let file = File::open(&args[1])?;
    let reader = BufReader::new(file);
    let total = reader.lines().fold(0, |acc, x| {
        let line = x.unwrap();
        if validate(&line) { acc + 1 } else { acc }
    });
    println!("total {}", total);

    Ok(())
}

fn validate(line: &String) -> bool {
    lazy_static! {
        static ref PAT: Regex = Regex::new(r"^(\d+)-(\d+) ([a-z]): ([a-z]+)$").unwrap();
    }
    match PAT.captures(&line) {
        Some(caps) => {
            let min: usize = caps[1].parse().unwrap();
            let max: usize = caps[2].parse().unwrap();
            let letter = caps[3].to_string();
            let pw = caps[4].to_string();
            let matches = pw.matches(&letter).collect::<Vec<&str>>().len();
            matches >= min && matches <= max
        },
        None => false,
    }
}