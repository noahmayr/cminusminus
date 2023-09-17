use std::fs;

use crate::generator::*;
use crate::lexer::*;
use crate::parser::*;
use clap::Parser as ClapParser;
use resolve_path::PathResolveExt;
mod context;
mod generator;
mod lexer;
mod parser;

#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    src: String,
    #[arg(short = 'x', long)]
    exec: bool,
    #[arg(short, long)]
    output: Option<String>,
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    let src_path = fs::canonicalize(&args.src).unwrap();
    let out_path = args.output.map_or_else(
        || src_path.with_extension("asm"),
        |out_path| out_path.resolve().into(),
    );
    let src = fs::read_to_string(&src_path).unwrap();

    let tokens = Lexer::tokenize(&src)?;
    let prog = Parser::parse(&tokens, &src)?;
    let compiled = Generator::generate(&prog, &src)?;

    fs::write(out_path, compiled).unwrap();
    Ok(())
}
