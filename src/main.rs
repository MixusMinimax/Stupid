use clap::Parser;

#[derive(Parser, Debug)]
#[clap(version = env!("CARGO_PKG_VERSION"))]
struct Arguments {
    #[clap(short, long)]
    input: Option<String>,
}

fn main() {
    let args = Arguments::parse();
    println!("{:?}", args);
}
