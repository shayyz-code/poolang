use poo::run_file_checked;
use std::env;
use std::process;

// speed
use std::time::Instant;

fn main() {
    let start = Instant::now();
    // Get the file path from command line arguments
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: <program> <file_path>");
        process::exit(1);
    }

    let file_path = &args[1];

    let result = match run_file_checked(file_path) {
        Ok(result) => result,
        Err(error) => {
            eprintln!("{error}");
            process::exit(1);
        }
    };

    match result {
        Some(value) => println!("Result: {:?}", value),
        None => print!(""),
    }

    let duration = start.elapsed();

    if args.contains(&"--speed".to_string()) {
        println!("\nInterpreted within: {:?}", duration);
    }
}
