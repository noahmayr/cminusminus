use std::fmt::Display;

use crate::parser::{Builtin, Expression, Program, Statement, Term};
use anyhow::Result;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Addr {
    Ptr(usize),
    Label(String),
}

impl From<Addr> for Data {
    fn from(val: Addr) -> Self {
        Data::Addr(val)
    }
}

impl Display for Addr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ptr(ptr) => write!(f, "#{}", ptr),
            Self::Label(label) => write!(f, "{}", label),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Data {
    Addr(Addr),
    Value(usize),
}

impl From<usize> for Data {
    fn from(val: usize) -> Self {
        Data::Value(val)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Int(String),
    String { addr: Addr, len: usize },
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int(_) => write!(f, "int"),
            Type::String { addr: _, len: _ } => write!(f, "string"),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct FileDescriptor(usize);
impl FileDescriptor {
    pub fn stdin() -> Self {
        Self(2)
    }
    pub fn stdout() -> Self {
        Self(1)
    }
    pub fn stderr() -> Self {
        Self(2)
    }
}

impl From<FileDescriptor> for Data {
    fn from(val: FileDescriptor) -> Self {
        val.0.into()
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Syscall {
    Exit {
        code: usize,
    },
    // Fork
    Read {
        fd: FileDescriptor,
        buf: Addr,
        bytes: usize,
    },
    Write {
        fd: FileDescriptor,
        buf: Addr,
        bytes: usize,
    },
}

impl Syscall {
    pub fn id(&self) -> usize {
        self.into()
    }
}

impl From<&Syscall> for usize {
    fn from(val: &Syscall) -> Self {
        match val {
            Syscall::Exit { code: _ } => 1,
            Syscall::Read {
                fd: _,
                buf: _,
                bytes: _,
            } => 3,
            Syscall::Write {
                fd: _,
                buf: _,
                bytes: _,
            } => 4,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Generator<'a> {
    program: &'a Program,
    output: String,
    string_lits: Vec<String>,
}

impl<'a> Generator<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self {
            program,
            output: String::new(),
            string_lits: vec![],
        }
    }

    pub fn generate(&mut self) -> Result<String> {
        self.push_line(".global _start");
        self.push_line(".align 2\n");
        self.push_line("_start:");
        for statement in self.program.0.iter() {
            self.generate_statement(statement)
        }
        if let Some(&Statement::Builtin(Builtin::Exit(_))) = self.program.0.last() {
        } else {
            self.generate_statement(&Statement::Builtin(Builtin::Exit(Expression::Term(
                Term::IntLiteral("0".into()),
            ))));
        }
        self.push_line("");
        self.generate_string_literals();
        Ok(self.output.clone())
    }

    fn generate_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Builtin(builtin) => self.generate_bultin(builtin),
        }
    }

    fn generate_bultin(&mut self, builtin: &Builtin) {
        match builtin {
            Builtin::Exit(expression) => {
                match self.generate_expression(expression) {
                    Type::Int(value) => self.push_syscall(Syscall::Exit {
                        code: value.parse().unwrap(),
                    }),
                    t => panic!("Unsupported type '{}' used in exit(). int expected", t),
                };
            }
            Builtin::Print(expression) => {
                match self.generate_expression(expression) {
                    Type::String { addr, len } => self.push_syscall(Syscall::Write {
                        fd: FileDescriptor::stdout(),
                        buf: addr,
                        bytes: len,
                    }),
                    t => panic!("Unsupported type '{}' used in print(). string expected", t),
                };
            }
        }
    }

    fn generate_expression(&mut self, expression: &Expression) -> Type {
        match expression {
            Expression::Term(term) => self.generate_term(term),
        }
    }

    fn generate_term(&mut self, term: &Term) -> Type {
        match term {
            Term::IntLiteral(value) => Type::Int(value.clone()),
            Term::StringLiteral(value) => {
                let label = match self.string_lits.iter().position(|it| it == value) {
                    Some(index) => str_label(index),
                    None => {
                        let label = str_label(self.string_lits.len());
                        self.string_lits.push(value.clone());
                        label
                    }
                };

                Type::String {
                    addr: Addr::Label(label),
                    len: value.as_bytes().len(),
                }
            }
        }
    }

    fn generate_string_literals(&mut self) {
        let literals = self.string_lits.clone();
        for (index, value) in literals.iter().enumerate() {
            let label = str_label(index);
            self.push_line(format!("{}: .asciz {}\n", label, value));
        }
    }

    fn push_syscall(&mut self, syscall: Syscall) {
        match &syscall {
            Syscall::Exit { code } => self.load_register(0, code.to_owned()),
            Syscall::Read {
                fd: _,
                buf: _,
                bytes: _,
            } => todo!(),
            Syscall::Write { fd, buf, bytes } => {
                self.load_register(0, fd.to_owned());
                self.load_register(1, buf.to_owned());
                self.load_register(2, bytes.to_owned());
            }
        }
        self.push_instr(format!("mov X16, #{}", &syscall.id()));
        self.push_instr("svc #0x80");
    }

    fn load_register<D: Into<Data>>(&mut self, register: usize, data: D) {
        match data.into() {
            Data::Addr(addr) => self.push_instr(format!("adr X{}, {}", register, addr)),
            Data::Value(value) => self.push_instr(format!("mov X{}, #{}", register, value)),
        }
    }

    fn push_instr<S: Display>(&mut self, line: S) {
        self.push_line(format!("  {}", line))
    }

    fn push_line<S: Display>(&mut self, line: S) {
        self.output.push_str(format!("{}\n", line).as_str())
    }
}

fn str_label(index: usize) -> String {
    format!("str_{}", index)
}
