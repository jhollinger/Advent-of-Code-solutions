use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    assert_eq!(2, args.len());

    let filename = &args[1];
    let raw_input = fs::read_to_string(filename).unwrap();
    let input: Vec<i32> = raw_input.trim().split("\n")
      .map(|l| l.parse().unwrap())
      .collect();

    let input_len = input.len();
    'outer: for i in 0..input_len {
        for ii in 0..input_len {
            if i != ii {
                let n = input[i];
                let m = input[ii];
                if n + m == 2020 {
                    let product = n * m;
                    println!("{}", product);
                    break 'outer;
                }
            }
        }
    }
}