mod parser;
mod type_analysis;
mod util;
use self::parser::CodeParser;
use self::type_analysis::TypeAnalyzer;
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(version = env!("CARGO_PKG_VERSION"))]
struct Arguments {
    #[clap()]
    input: String,
}

fn main() {
    let args = Arguments::parse();
    let parser = CodeParser::new(args.input);
    let parsed = parser.parse().unwrap();
    let type_analyzer = TypeAnalyzer::new(parsed);
    let typed = type_analyzer.analyze().unwrap();
    println!("\n{}", typed.program);
}
