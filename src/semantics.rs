use std::{fmt::Display, rc::Rc};

use crate::{context::*, parser};
use arcstr::ArcStr;
use miette::{MietteDiagnostic, Result, SourceSpan};

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Type {
    Uint64,
    String,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Uint64 => "uint64",
                Type::String => "string",
            }
        )
    }
}

pub trait TypeOf {
    fn get_type(&self) -> Type;
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Var {
    pub typ: Type,
    pub ident: ArcStr,
}

impl TypeOf for Var {
    fn get_type(&self) -> Type {
        self.typ
    }
}

impl<T: TypeOf> TypeOf for Src<T> {
    fn get_type(&self) -> Type {
        self.inner().get_type()
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodeExpr {
    Lit { typ: Type, val: ArcStr },
    Var(Var),
}
impl TypeOf for NodeExpr {
    fn get_type(&self) -> Type {
        match self {
            NodeExpr::Lit { typ, val } => *typ,
            NodeExpr::Var(var) => var.get_type(),
        }
    }
}
pub type SrcExpr = Src<NodeExpr>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodeBuiltin {
    Exit(SrcExpr),
    Print(SrcExpr),
}
pub type SrcBuiltin = Src<NodeBuiltin>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct NodeScope {
    pub statements: Vec<SrcStmt>,
}
pub type SrcScope = Src<NodeScope>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodeStmt {
    Builtin(SrcBuiltin),
    Let {
        typ: Type,
        ident: ArcStr,
        expr: SrcExpr,
    },
    Scope(SrcScope),
}
pub type SrcStmt = Src<NodeStmt>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Ast(pub SrcScope);

#[derive(Debug)]
pub struct SemanticAnalyzer<'a> {
    parse_tree: &'a parser::ParseTree,
    context: Rc<Context>,
    vars: Vec<Var>,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(parse_tree: &'a parser::ParseTree, context: Rc<Context>) -> Self {
        Self {
            parse_tree,
            context,
            vars: vec![],
        }
    }

    pub fn analyze(parse_tree: &'a parser::ParseTree, context: Rc<Context>) -> Result<Ast> {
        Self::new(parse_tree, context).run()
    }

    pub fn run(&mut self) -> Result<Ast> {
        let res = Ast(self.analyze_scope(&self.parse_tree.0)?);

        Ok(res)
    }

    fn analyze_statement(&mut self, statement: &parser::SrcStmt) -> Result<Option<SrcStmt>> {
        Ok(Some(match statement.inner() {
            parser::NodeStmt::Builtin(builtin) => {
                let Some(builtin) = self.anaylze_builtin(builtin)? else {
                return Ok(None);
            };
                Src::new(NodeStmt::Builtin(builtin), statement)
            }
            parser::NodeStmt::Let { ident, expr } => {
                let Some(expr) = self.analyze_expression(expr) else {return Ok(None);};
                let var = Var {
                    typ: expr.get_type(),
                    ident: ident.clone(),
                };
                self.vars.push(var);
                Src::new(
                    NodeStmt::Let {
                        typ: expr.get_type(),
                        ident: ident.clone(),
                        expr,
                    },
                    statement,
                )
            }
            parser::NodeStmt::Scope(scope) => {
                Src::new(NodeStmt::Scope(self.analyze_scope(scope)?), statement)
            }
        }))
    }

    fn analyze_scope(&mut self, scope: &parser::SrcScope) -> Result<SrcScope> {
        let mut statements: Vec<SrcStmt> = vec![];
        let var_len = self.vars.len();
        for statement in scope.statements.iter() {
            if let Some(statement) = self.analyze_statement(statement)? {
                statements.push(statement);
            }
        }
        self.vars.truncate(var_len);
        Ok(Src::new(NodeScope { statements }, scope))
    }

    fn anaylze_builtin(&mut self, builtin: &parser::SrcBuiltin) -> Result<Option<SrcBuiltin>> {
        match builtin.inner() {
            parser::NodeBuiltin::Exit(expr) => {
                let Some(expr) = self.analyze_expression(expr) else {return Ok(None);};
                if !self.expect_type(&expr, Type::Uint64) {
                    return Ok(None);
                }
                Ok(Some(Src::new(NodeBuiltin::Exit(expr), builtin)))
            }
            parser::NodeBuiltin::Print(expr) => {
                let Some(expr) = self.analyze_expression(expr) else {return Ok(None);};
                if !self.expect_type(&expr, Type::String) {
                    return Ok(None);
                }
                Ok(Some(Src::new(NodeBuiltin::Print(expr), builtin)))
            }
        }
    }

    fn analyze_expression(&mut self, expr: &parser::SrcExpr) -> Option<SrcExpr> {
        Some(match expr.inner() {
            parser::NodeExpr::IntLiteral(value) => Src::new(
                NodeExpr::Lit {
                    typ: Type::Uint64,
                    val: value.clone(),
                },
                expr,
            ),
            parser::NodeExpr::StringLiteral(val) => Src::new(
                NodeExpr::Lit {
                    typ: Type::String,
                    val: val.clone(),
                },
                expr,
            ),
            parser::NodeExpr::Var(name) => {
                let Some(var) = self.get_var(name) else {
                    self.context.error(Error::UndefinedVariable { span: expr.into(), name: name.clone() });
                    return None;
                };
                Src::new(NodeExpr::Var(var.clone()), expr)
            }
        })
    }

    fn expect_type(&self, expr: &SrcExpr, typ: Type) -> bool {
        if expr.get_type() != typ {
            self.context.error(Error::TypeError {
                expr: expr.clone(),
                expected: typ,
            });
            false
        } else {
            true
        }
    }

    fn get_var(&self, name: &ArcStr) -> Option<&Var> {
        self.vars.iter().rev().find(|var| &var.ident == name)
    }
}

#[derive(Debug)]
enum Error {
    TypeError { expr: SrcExpr, expected: Type },
    UndefinedVariable { span: SourceSpan, name: ArcStr },
}

impl From<Error> for MietteDiagnostic {
    fn from(value: Error) -> Self {
        match value {
            Error::TypeError { expected, expr } => MietteDiagnostic::new(format!(
                "Unexpected type '{}', expected '{}'",
                expr.get_type(),
                expected
            ))
            .with_code("cmm::semantics::type_error")
            .add_label(
                format!("expected {}, got {}", expected, expr.get_type()),
                expr,
            ),
            Error::UndefinedVariable { span, name } => {
                MietteDiagnostic::new(format!("Undefined variable '{}'", name,))
                    .with_code("cmm::semantics::undefined_variable")
                    .add_label(format!("undefined variable '{}'", name), span)
            }
        }
    }
}
