use abstract_asm::Compiler;
use clap::Parser;
use evaluator::Evaluator;
use parser::CodeParser;
use type_analysis::TypeAnalyzer;

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
    println!(
        "\n====vvv=====[Program]=====vvv====\n{}\n====^^^=====[Program]=====^^^====\n",
        typed.program
    );
    let evaluator = Evaluator::new(typed);
    let evaluated = evaluator.evaluate().unwrap();
    println!(
        "\n====vvv====[Evaluated]====vvv====\n{}\n====^^^====[Evaluated]====^^^====\n",
        evaluated.program
    );
    let compiler = Compiler::new(evaluated);
    let intermediary = compiler.compile().unwrap();
    println!("{:?}", intermediary);
}
