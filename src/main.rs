#![recursion_limit = "256"]
use std::fs;
use std::rc::Rc;

use crate::lexer::*;
use crate::parser::*;
use clap::Parser as ClapParser;
use context::Context;
use llvm::Llvm;
use resolve_path::PathResolveExt;
use semantics::SemanticAnalyzer;
mod context;
mod lexer;
mod llvm;
mod parser;
mod semantics;

#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    src: String,
    #[arg(short = 'x', long, conflicts_with = "output")]
    exec: bool,
    #[arg(short, long)]
    output: Option<String>,
}

fn main() -> miette::Result<()> {
    miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::new()
                .terminal_links(true)
                .unicode(true)
                .context_lines(3)
                .tab_width(4)
                .rgb_colors(miette::RgbColors::Always)
                .build(),
        )
    }))?;
    let args = Args::parse();
    let src_path = fs::canonicalize(&args.src).unwrap();
    let out_path = args.output.map_or_else(
        || src_path.with_extension("ll"),
        |out_path| out_path.resolve().into(),
    );
    let src = fs::read_to_string(&src_path).unwrap();
    let context = Rc::new(Context::new(src_path.to_string_lossy(), src));

    let tokens = Lexer::tokenize(context.clone())?;
    let parse_tree = Parser::parse(&tokens, context.clone())?;

    let ast = context.result(SemanticAnalyzer::analyze(&parse_tree, context.clone())?)?;

    let ctx = inkwell::context::Context::create();
    if args.exec {
        println!();
        Llvm::execute(&ast, context.clone(), &ctx);
    }

    let compiled = Llvm::generate_ir(&ast, context.clone(), &ctx);

    fs::write(out_path, compiled).unwrap();
    Ok(())
}
