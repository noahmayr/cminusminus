use std::{fmt::Display, rc::Rc};

use crate::{context::*, parser::*};
use arcstr::ArcStr;
use lazy_static::lazy_static;
use miette::{MietteDiagnostic, Result};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Addr {
    Label(ArcStr),
}

impl From<Addr> for Data {
    fn from(val: Addr) -> Self {
        Data::Addr(val)
    }
}

impl Display for Addr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Label(label) => write!(f, "{}", label),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Data {
    Addr(Addr),
    Value(usize),
    Stack(usize),
    Register(Reg),
}

impl From<usize> for Data {
    fn from(val: usize) -> Self {
        Data::Value(val)
    }
}

impl From<&Reg> for Data {
    fn from(value: &Reg) -> Self {
        Data::Register(*value)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum Type {
    Int,
    String,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Int => 8,
            Type::String => 16,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Int => "int",
                Type::String => "string",
            }
        )
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Var {
    ident: ArcStr,
    stack_loc: usize,
    typ: Type,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypedExpr {
    IntLit(ArcStr),
    Var(Var),
    StringLit { addr: Addr, len: usize },
}

pub type SrcTypedExpr = Src<TypedExpr>;

impl Display for TypedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedExpr::IntLit(_) => write!(f, "int"),
            TypedExpr::Var(var) => var.typ.fmt(f),
            TypedExpr::StringLit { addr: _, len: _ } => write!(f, "string"),
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

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum RegId {
    ZR,
    R(usize),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Reg {
    W(RegId),
    X(RegId),
    SP,
    LR,
}

impl Reg {
    pub fn w(id: usize) -> Self {
        Reg::W(RegId::R(id))
    }

    pub fn wzr() -> Self {
        Reg::W(RegId::ZR)
    }
    pub fn x(id: usize) -> Self {
        Reg::X(RegId::R(id))
    }
    pub fn xzr() -> Self {
        Reg::X(RegId::ZR)
    }
    pub fn sp() -> Self {
        Reg::SP
    }
    pub fn lr() -> Self {
        Reg::LR
    }

    pub fn size(&self) -> usize {
        match self {
            Reg::W(_) => 4,
            Reg::X(_) => 8,
            Reg::SP => 8,
            Reg::LR => 8,
        }
    }
}

lazy_static! {
    static ref SP: Reg = Reg::sp();
    static ref LR: Reg = Reg::lr();
    static ref XZR: Reg = Reg::xzr();
    static ref X0: Reg = Reg::x(0);
    static ref X1: Reg = Reg::x(1);
    static ref X2: Reg = Reg::x(2);
    static ref X3: Reg = Reg::x(3);
    static ref X4: Reg = Reg::x(4);
    static ref X5: Reg = Reg::x(5);
    static ref X6: Reg = Reg::x(6);
    static ref X7: Reg = Reg::x(7);
    static ref X8: Reg = Reg::x(8);
    static ref X9: Reg = Reg::x(9);
    static ref X10: Reg = Reg::x(10);
    static ref X11: Reg = Reg::x(11);
    static ref X12: Reg = Reg::x(21);
    static ref X13: Reg = Reg::x(13);
    static ref X14: Reg = Reg::x(14);
    static ref X15: Reg = Reg::x(15);
    static ref X16: Reg = Reg::x(16);
    static ref X17: Reg = Reg::x(17);
    static ref X18: Reg = Reg::x(18);
    static ref X19: Reg = Reg::x(19);
    static ref X20: Reg = Reg::x(20);
    static ref X21: Reg = Reg::x(21);
    static ref X22: Reg = Reg::x(21);
    static ref X23: Reg = Reg::x(23);
    static ref X24: Reg = Reg::x(24);
    static ref X25: Reg = Reg::x(25);
    static ref X26: Reg = Reg::x(26);
    static ref X27: Reg = Reg::x(27);
    static ref X28: Reg = Reg::x(28);
    static ref X29: Reg = Reg::x(29);
    static ref X30: Reg = Reg::x(30);
    static ref WZR: Reg = Reg::wzr();
    static ref W0: Reg = Reg::w(0);
    static ref W1: Reg = Reg::w(1);
    static ref W2: Reg = Reg::w(2);
    static ref W3: Reg = Reg::w(3);
    static ref W4: Reg = Reg::w(4);
    static ref W5: Reg = Reg::w(5);
    static ref W6: Reg = Reg::w(6);
    static ref W7: Reg = Reg::w(7);
    static ref W8: Reg = Reg::w(8);
    static ref W9: Reg = Reg::w(9);
    static ref W10: Reg = Reg::w(10);
    static ref W11: Reg = Reg::w(11);
    static ref W12: Reg = Reg::w(21);
    static ref W13: Reg = Reg::w(13);
    static ref W14: Reg = Reg::w(14);
    static ref W15: Reg = Reg::w(15);
    static ref W16: Reg = Reg::w(16);
    static ref W17: Reg = Reg::w(17);
    static ref W18: Reg = Reg::w(18);
    static ref W19: Reg = Reg::w(19);
    static ref W20: Reg = Reg::w(20);
    static ref W21: Reg = Reg::w(21);
    static ref W22: Reg = Reg::w(21);
    static ref W23: Reg = Reg::w(23);
    static ref W24: Reg = Reg::w(24);
    static ref W25: Reg = Reg::w(25);
    static ref W26: Reg = Reg::w(26);
    static ref W27: Reg = Reg::w(27);
    static ref W28: Reg = Reg::w(28);
    static ref W29: Reg = Reg::w(29);
    static ref W30: Reg = Reg::w(30);
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Reg::W(id) => match id {
                    RegId::ZR => "WZR".into(),
                    RegId::R(n) if n > &30 => return Err(std::fmt::Error),
                    RegId::R(n) => format!("W{}", n),
                },
                Reg::X(id) => match id {
                    RegId::ZR => "XZR".into(),
                    RegId::R(n) if n > &30 => return Err(std::fmt::Error),
                    RegId::R(n) => format!("X{}", n),
                },
                Reg::SP => "SP".into(),
                Reg::LR => "LR".into(),
            }
        )
    }
}

#[derive(Debug)]
pub struct Generator<'a> {
    program: &'a Program,
    context: Rc<Context>,
    output: String,
    string_lits: Vec<ArcStr>,
    aligned_stack_size: usize,
    stack_size: usize,
    vars: Vec<Var>,
}

impl<'a> Generator<'a> {
    pub fn new(program: &'a Program, context: Rc<Context>) -> Self {
        Self {
            program,
            context,
            output: String::new(),
            string_lits: vec![],
            aligned_stack_size: 0,
            stack_size: 0,
            vars: vec![],
        }
    }

    pub fn generate(program: &'a Program, context: Rc<Context>) -> Result<String> {
        Self::new(program, context).run()
    }

    pub fn run(&mut self) -> Result<String> {
        self.push_line(".global _start");
        self.push_line(".align 2\n");
        self.push_line("_start:");
        // self.resize_aligned_stack(self.stack_size + 64);
        for statement in self.program.0.iter() {
            self.generate_statement(statement)?
        }

        self.push_syscall(Syscall::Exit { code: 0 });

        self.push_line("");
        self.generate_string_literals();

        self.context.result(self.output.clone())
    }

    fn generate_statement(&mut self, statement: &SrcStmt) -> Result<()> {
        let stmt_span = statement.span();
        let line = self
            .context
            .src()
            .substr(stmt_span.offset()..=(stmt_span.offset() + stmt_span.len()))
            .trim()
            .to_string();
        if line.as_bytes().iter().filter(|&&c| c == b'\n').count() == 0 {
            self.push_instr(format!("; {}", line,));
        }

        match statement.inner() {
            NodeStmt::Builtin(builtin) => self.generate_bultin(builtin)?,
            NodeStmt::Let { ident, expr } => {
                let old_type = self.get_var(ident).map(|v| v.typ.clone());
                let Some(expr) = self.generate_expression(expr) else {return Ok(());};
                match expr.inner() {
                    TypedExpr::IntLit(value) => {
                        let typ = Type::Int;
                        if old_type.map_or(false, |old_type| old_type != typ) {
                            self.context.error(GenerationError::TypeError {
                                expr: expr.clone(),
                                expected: arcstr::literal!("int"),
                            });
                            return Ok(());
                        }
                        self.load_register(&X0, value.parse::<usize>().unwrap());
                        let stack_loc = self.push_reg(&X0);
                        self.vars.push(Var {
                            ident: ident.clone(),
                            stack_loc,
                            typ,
                        });
                    }
                    TypedExpr::Var(Var {
                        ident: _,
                        typ,
                        stack_loc,
                    }) => {
                        let stack_loc = match typ {
                            Type::Int => {
                                self.load_reg(&X0, *stack_loc);
                                self.push_reg(&X0)
                            }
                            Type::String => {
                                self.load_regp(&X0, &X1, *stack_loc);
                                self.push_regp(&X0, &X1)
                            }
                        };
                        if old_type.map_or(false, |old_type| &old_type != typ) {
                            self.context.error(GenerationError::TypeError {
                                expr: expr.clone(),
                                expected: typ.to_string().into(),
                            });
                            return Ok(());
                        }
                        self.vars.push(Var {
                            ident: ident.clone(),
                            stack_loc,
                            typ: typ.clone(),
                        });
                    }
                    TypedExpr::StringLit { addr, len } => {
                        let typ = Type::String;
                        if old_type.map_or(false, |old_type| old_type != typ) {
                            self.context.error(GenerationError::TypeError {
                                expr: expr.clone(),
                                expected: arcstr::literal!("int"),
                            });
                            return Ok(());
                        }
                        self.load_register(&X0, addr.clone());
                        // idk why I need to subtract 2 or 3 here, but it works
                        self.load_register(&X1, *len - 2);
                        let stack_loc = self.push_regp(&X0, &X1);
                        self.vars.push(Var {
                            ident: ident.clone(),
                            stack_loc,
                            typ,
                        });
                    }
                }
            }
            NodeStmt::Scope(statements) => {
                self.push_instr("; {");
                let stack_size = self.stack_size;
                let var_size = self.vars.len();
                for statement in statements.iter() {
                    self.generate_statement(statement)?
                }
                self.shrink_stack(self.stack_size - stack_size);
                self.vars.truncate(var_size);
                self.push_instr("; }");
            }
        };
        Ok(())
    }

    fn generate_bultin(&mut self, builtin: &SrcBuiltin) -> Result<()> {
        match builtin.inner() {
            NodeBuiltin::Exit(expression) => {
                let Some(expr) = self.generate_expression(expression) else {return Ok(());};

                match expr.inner() {
                    TypedExpr::IntLit(value) => self.push_syscall(Syscall::Exit {
                        code: value.parse().unwrap(),
                    }),
                    TypedExpr::Var(Var {
                        ident: _,
                        typ: Type::Int,
                        stack_loc,
                    }) => {
                        self.load_register(&X0, Data::Stack(*stack_loc));
                        self.load_register(&X16, 1);
                        self.push_instr("svc 0");
                    }
                    _ => {
                        self.context.error(GenerationError::TypeError {
                            expr,
                            expected: arcstr::literal!("int"),
                        });
                    }
                };
            }
            NodeBuiltin::Print(expression) => {
                let Some(expr) = self.generate_expression(expression) else {return Ok(());};
                match expr.inner() {
                    TypedExpr::StringLit { addr, len } => self.push_syscall(Syscall::Write {
                        fd: FileDescriptor::stdout(),
                        buf: addr.clone(),
                        // idk why I need to subtract 2 or 3 here, but it works
                        bytes: *len - 2,
                    }),
                    TypedExpr::Var(Var {
                        ident: _,
                        typ: Type::String,
                        stack_loc,
                    }) => {
                        self.load_register(&X0, FileDescriptor::stdout());
                        self.load_regp(&X1, &X2, *stack_loc);
                        self.load_register(&X16, 4);
                        self.push_instr("svc 0");
                    }
                    _ => {
                        self.context.error(GenerationError::TypeError {
                            expr,
                            expected: arcstr::literal!("string"),
                        });
                    }
                };
            }
        };
        Ok(())
    }

    fn generate_expression(&mut self, expr: &SrcExpr) -> Option<SrcTypedExpr> {
        Some(match expr.inner() {
            NodeExpr::IntLiteral(value) => Src::new(TypedExpr::IntLit(value.clone()), expr),
            NodeExpr::StringLiteral(value) => {
                let label = match self.string_lits.iter().position(|it| it == value) {
                    Some(index) => str_label(index),
                    None => {
                        let label = str_label(self.string_lits.len());
                        self.string_lits.push(value.clone());
                        label
                    }
                };

                Src::new(
                    TypedExpr::StringLit {
                        addr: Addr::Label(label),
                        len: value.as_bytes().len(),
                    },
                    expr,
                )
            }
            NodeExpr::Var(name) => {
                let Some(var) = self.get_var(name) else {
                    self.context.error(GenerationError::UndefinedVariable { expr: expr.clone(), name: name.clone() });
                    return None;
                };
                Src::new(TypedExpr::Var(var.clone()), expr)
            }
        })
    }

    fn generate_string_literals(&mut self) {
        let literals = self.string_lits.clone();
        for (index, value) in literals.iter().enumerate() {
            let label = str_label(index);
            self.push_line(".balign 4");
            self.push_line(format!("{}: .asciz {}\n", label, value));
        }
    }

    fn push_syscall(&mut self, syscall: Syscall) {
        match &syscall {
            Syscall::Exit { code } => self.load_register(&X0, code.to_owned()),
            Syscall::Read {
                fd: _,
                buf: _,
                bytes: _,
            } => todo!(),
            Syscall::Write { fd, buf, bytes } => {
                self.load_register(&X0, fd.to_owned());
                self.load_register(&X1, buf.to_owned());
                self.load_register(&X2, bytes.to_owned());
            }
        }
        self.load_register(&X16, syscall.id());

        self.push_instr("svc #0x80");
    }

    fn load_register<D: Into<Data>>(&mut self, register: &Reg, data: D) {
        match data.into() {
            Data::Addr(addr) => self.push_instr(format!("adr {}, {}", register, addr)),
            Data::Value(value) => self.push_instr(format!("mov {}, #{}", register, value)),
            Data::Stack(loc) => self.load_reg(register, loc),
            Data::Register(reg) => self.push_instr(format!("mov {}, {}", register, reg)),
        }
    }

    fn grow_stack(&mut self, amount: usize) {
        self.stack_size += amount;
        self.resize_aligned_stack(self.stack_size);
    }

    fn shrink_stack(&mut self, amount: usize) {
        self.stack_size -= amount;
        self.resize_aligned_stack(self.stack_size);
    }

    fn resize_aligned_stack(&mut self, stack_size: usize) {
        let new_aligned = ((stack_size + 15) / 16) * 16;
        if new_aligned > self.aligned_stack_size {
            self.push_instr(format!(
                "sub sp, sp, #{}",
                new_aligned - self.aligned_stack_size
            ));
            self.aligned_stack_size = new_aligned
        } else if new_aligned < self.aligned_stack_size {
            self.push_instr(format!(
                "add sp, sp, #{}",
                self.aligned_stack_size - new_aligned
            ));
            self.aligned_stack_size = new_aligned
        }
    }

    fn push_reg(&mut self, reg: &Reg) -> usize {
        let loc = self.allocate_stack_loc(reg.size());

        self.push_instr(format!(
            "str {}, [SP, #{}]",
            reg,
            self.offset_from_stack_loc(loc)
        ));
        loc
    }

    fn push_regp(&mut self, reg1: &Reg, reg2: &Reg) -> usize {
        let loc = self.allocate_stack_loc(reg1.size() + reg2.size());
        self.push_instr(format!(
            "stp {}, {}, [SP, #{}]",
            reg1,
            reg2,
            self.offset_from_stack_loc(loc)
        ));
        loc
    }

    fn pop_reg(&mut self, reg: &Reg) {
        self.load_reg(reg, self.stack_size + reg.size());
        self.shrink_stack(reg.size());
    }

    fn pop_regp(&mut self, reg1: &Reg, reg2: &Reg) {
        let size = reg1.size() + reg2.size();
        self.load_regp(reg1, reg2, self.stack_size + size);
        self.shrink_stack(size);
    }

    fn load_reg(&mut self, reg: &Reg, loc: usize) {
        self.push_instr(format!(
            "add {}, {}, {}",
            reg,
            *SP,
            self.offset_from_stack_loc(loc)
        ));
        self.push_instr(format!("ldr {}, [{}]", reg, reg,));
    }

    fn load_regp(&mut self, reg1: &Reg, reg2: &Reg, loc: usize) {
        self.push_instr(format!(
            "add {}, {}, {}",
            reg1,
            *SP,
            self.offset_from_stack_loc(loc)
        ));
        self.push_instr(format!("ldp {}, {}, [{}]", reg1, reg2, reg1,));
    }

    fn allocate_stack_loc(&mut self, size: usize) -> usize {
        self.grow_stack(size);
        self.stack_size
    }

    fn offset_from_stack_loc(&self, loc: usize) -> usize {
        self.aligned_stack_size - loc
    }

    fn push_instr<S: Display>(&mut self, line: S) {
        self.push_line(format!("  {}", line))
    }

    fn push_line<S: Display>(&mut self, line: S) {
        self.output.push_str(format!("{}\n", line).as_str())
    }

    fn get_var(&self, name: &ArcStr) -> Option<&Var> {
        self.vars.iter().rev().find(|var| &var.ident == name)
    }
}

fn str_label(index: usize) -> ArcStr {
    format!("str_{}", index).into()
}

#[derive(Debug)]
enum GenerationError {
    TypeError {
        expr: SrcTypedExpr,
        expected: ArcStr,
    },
    UndefinedVariable {
        expr: SrcExpr,
        name: ArcStr,
    },
}

impl From<GenerationError> for MietteDiagnostic {
    fn from(value: GenerationError) -> Self {
        match value {
            GenerationError::TypeError { expected, expr } => MietteDiagnostic::new(format!(
                "Unexpected type '{}', expected '{}'",
                expr.inner(),
                expected
            ))
            .with_code("cmm::generate::type_error")
            .add_label(format!("expected {}, got {}", expected, expr.inner()), expr),
            GenerationError::UndefinedVariable { expr, name: ident } => {
                MietteDiagnostic::new(format!("Undefined variable '{}'", ident,))
                    .with_code("cmm::generate::undefined_variable")
                    .add_label(format!("undefined variable '{}'", ident), expr)
            }
        }
    }
}
