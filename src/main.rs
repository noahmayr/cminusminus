use std::fs;

use crate::generator::*;
use crate::lexer::*;
use crate::parser::*;
use clap::Parser as ClapParser;
use resolve_path::PathResolveExt;

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

fn main() {
    let args = Args::parse();
    let src_path = fs::canonicalize(&args.src).unwrap();
    let out_path = args.output.map_or_else(
        || src_path.with_extension("asm"),
        |out_path| out_path.resolve().into(),
    );
    let src = fs::read_to_string(&src_path).unwrap();
    let tokens = tokenize(&src);
    let program = parse(&tokens);
    let asm = generate(&program);

    fs::write(&out_path, &asm).unwrap();
}

fn tokenize(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source);

    lexer.tokenize()
}

fn parse(tokens: &Vec<Token>) -> Program {
    let mut parser = Parser::new(tokens);
    parser.parse().unwrap()
}

fn generate(program: &Program) -> String {
    let mut generator = Generator::new(program);
    generator.generate().unwrap()
}

#[cfg(test)]
mod tests {
    use crate::{lexer::*, parse, parser::*, tokenize};

    #[test]
    fn exits_with_code_0() {
        let source = "exit(0);";
        let tokens = tokenize(source);
        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::Exit),
                Token::OpenParen,
                Token::IntLiteral("0".into()),
                Token::CloseParen,
                Token::Semicolon
            ]
        );
        let program = parse(&tokens);
        assert_eq!(
            program,
            Program(vec![Statement::Builtin(Builtin::Exit(Expression::Term(
                Term::IntLiteral("0".into())
            )))])
        )
    }
}
