use clap::Parser;

mod parser;
use crate::parser::CodeParser;

#[derive(Parser, Debug)]
#[clap(version = env!("CARGO_PKG_VERSION"))]
struct Arguments {
    #[clap()]
    input: String,
}

fn main() {
    let args = Arguments::parse();
    println!("{:?}", args);
    let parser = CodeParser::new(args.input);
    let result = parser.parse();
    println!("{:?}", result)
}
