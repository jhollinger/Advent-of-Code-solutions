use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    assert_eq!(2, args.len());

    let input = parse_input(&args[1]);
    let len = input.len();

    'outer: for a in 0..len {
        for b in 0..len {
            if b != a {
                for c in 0..len {
                    let n = input[a];
                    let m = input[b];
                    if c != b && c != a && n + m < 2020 {
                        let i = input[c];
                        if n + m + i == 2020 {
                            let product = n * m * i;
                            println!("{}", product);
                            break 'outer;
                        }
                    }
                }
            }
        }
    }
}

fn parse_input(filename: &str) -> Vec<i32> {
    let raw_input = fs::read_to_string(filename).unwrap();
    raw_input.trim()
        .split("\n")
        .map(|l| l.parse::<i32>().unwrap())
        .collect()
}
