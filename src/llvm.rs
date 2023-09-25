use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

use crate::{
    context::Context,
    semantics::{self, *},
};
use arcstr::ArcStr;
use inkwell::{
    builder::Builder,
    context::Context as Ctx,
    execution_engine::JitFunction,
    module::{Linkage, Module},
    values::{BasicValue, PointerValue},
    AddressSpace, OptimizationLevel,
};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Var<'ctx> {
    ident: ArcStr,
    ptr: PointerValue<'ctx>,
    typ: Type,
}

#[derive(Debug)]
pub struct Llvm<'a, 'ctx> {
    ast: &'a Ast,
    ctx: &'ctx Ctx,
    module: RefCell<Module<'ctx>>,
    builder: RefCell<Builder<'ctx>>,
    string_lits: RefCell<HashMap<ArcStr, PointerValue<'ctx>>>,
    vars: RefCell<Vec<Var<'ctx>>>,
}

type MainFunc = unsafe extern "C" fn() -> i32;

impl<'a, 'ctx> Llvm<'a, 'ctx> {
    pub fn new(ast: &'a Ast, context: Rc<Context>, ctx: &'ctx Ctx) -> Self {
        let mod_path = PathBuf::from(context.name().as_str());
        let module = ctx.create_module(
            mod_path
                .with_extension("")
                .file_name()
                .and_then(|f| f.to_str())
                .unwrap_or(context.name().as_str()),
        );
        module.set_source_file_name(context.name().as_str());
        Self {
            ast,
            module: RefCell::new(module),
            builder: RefCell::new(ctx.create_builder()),
            ctx,
            string_lits: RefCell::new(HashMap::new()),
            vars: RefCell::new(vec![]),
        }
    }

    pub fn execute(ast: &'a Ast, context: Rc<Context>, ctx: &'ctx Ctx) -> ! {
        Self::new(ast, context, ctx).run_jit()
    }

    pub fn generate_ir(ast: &'a Ast, context: Rc<Context>, ctx: &'ctx Ctx) -> String {
        Self::new(ast, context, ctx).get_ir()
    }

    fn run_jit(&mut self) -> ! {
        self.codegen();
        let jit = self
            .module
            .borrow()
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main_func: JitFunction<MainFunc> = unsafe { jit.get_function("main").unwrap() };
        std::process::exit(unsafe { main_func.call() })
    }

    fn get_ir(&mut self) -> String {
        self.codegen();
        self.module.borrow().to_string()
    }

    fn codegen(&mut self) {
        let i32_type = self.ctx.i32_type();
        let char_array_type = self
            .ctx
            .i8_type()
            .ptr_type(AddressSpace::default())
            .ptr_type(AddressSpace::default());
        let main_type = i32_type.fn_type(&[i32_type.into(), char_array_type.into()], false);
        let main = self
            .module
            .borrow_mut()
            .add_function("main", main_type, None);
        let basic_block = self.ctx.append_basic_block(main, "entry");

        self.builder.borrow_mut().position_at_end(basic_block);
        self.generate_scope(&self.ast.0);
        let zero = i32_type.const_zero();

        self.builder.borrow_mut().build_return(Some(&zero));
    }

    fn generate_scope(&mut self, scope: &SrcScope) {
        let var_size = self.vars.borrow().len();
        for statement in scope.statements.iter() {
            self.generate_statement(statement)
        }
        self.vars.borrow_mut().truncate(var_size);
    }

    fn generate_statement(&mut self, statement: &SrcStmt) {
        match statement.inner() {
            NodeStmt::Builtin(builtin) => self.generate_bultin(builtin),
            NodeStmt::Let { typ, ident, expr } => {
                let llvm_expr = self.generate_expression(expr);

                match typ {
                    Type::Int32 => {
                        let i32_type = self.ctx.i32_type();
                        let ptr = self.builder.borrow().build_alloca(i32_type, ident);
                        self.builder.borrow().build_store(ptr, llvm_expr);

                        self.vars.borrow_mut().push(Var {
                            ident: ident.clone(),
                            ptr,
                            typ: *typ,
                        });
                    }
                    Type::String => {
                        let i32_type = self.ctx.i8_type().ptr_type(AddressSpace::default());
                        let ptr: inkwell::values::PointerValue<'_> =
                            self.builder.borrow().build_alloca(i32_type, ident);
                        self.builder.borrow().build_store(ptr, llvm_expr);

                        self.vars.borrow_mut().push(Var {
                            ident: ident.clone(),
                            ptr,
                            typ: *typ,
                        });
                    }
                }
            }
            NodeStmt::Scope(scope) => {
                self.generate_scope(scope);
            }
        };
    }

    fn generate_bultin(&self, builtin: &SrcBuiltin) {
        match builtin.inner() {
            NodeBuiltin::Exit(expr) => {
                let module = self.module.borrow_mut();
                let fn_exit = match module.get_function("exit") {
                    Some(x) => x,
                    None => {
                        let fn_type = self
                            .ctx
                            .void_type()
                            .fn_type(&[self.ctx.i32_type().into()], true);
                        let fn_exit = module.add_function("exit", fn_type, Some(Linkage::External));
                        fn_exit.set_call_conventions(0); // CallingConv:C, why is this not an enum?
                        fn_exit
                    }
                };
                let expr = self.generate_expression(expr);

                let builder = self.builder.borrow_mut();
                builder.build_call(fn_exit, &[expr.into()], "call");
            }
            NodeBuiltin::Print(expr) => {
                let module = self.module.borrow_mut();
                let fn_printf = match module.get_function("printf") {
                    Some(x) => x,
                    None => {
                        let char_ptr_type = self.ctx.i8_type().ptr_type(AddressSpace::default());
                        let fn_type = self.ctx.i32_type().fn_type(&[char_ptr_type.into()], true);
                        let fn_printf =
                            module.add_function("printf", fn_type, Some(Linkage::External));
                        fn_printf.set_call_conventions(0); // CallingConv:C, why is this not an enum?
                        fn_printf
                    }
                };
                let expr = self.generate_expression(expr);

                let fmt_ptr = self.string_literal(&arcstr::literal!("%s"));
                self.builder.borrow_mut().build_call(
                    fn_printf,
                    &[fmt_ptr.as_basic_value_enum().into(), expr.into()],
                    "call",
                );
            }
        };
    }

    fn generate_expression(&self, expr: &SrcExpr) -> inkwell::values::BasicValueEnum {
        match expr.inner() {
            NodeExpr::Lit { typ, val } => match typ {
                Type::Int32 => {
                    let i32_type = self.ctx.i32_type();
                    i32_type
                        .const_int_from_string(val, inkwell::types::StringRadix::Decimal)
                        .unwrap()
                        .as_basic_value_enum()
                }
                Type::String => self.string_literal(val),
            },
            NodeExpr::Var(var) => match var.get_type() {
                Type::Int32 => {
                    let var = self.get_var(var);
                    self.builder
                        .borrow()
                        .build_load(self.ctx.i32_type(), var.ptr, "load")
                        .as_basic_value_enum()
                }
                Type::String => {
                    let var = self.get_var(var);
                    self.builder
                        .borrow()
                        .build_load(
                            self.ctx.i8_type().ptr_type(AddressSpace::default()),
                            var.ptr,
                            "load",
                        )
                        .as_basic_value_enum()
                }
            },
        }
    }

    fn string_literal(&self, val: &ArcStr) -> inkwell::values::BasicValueEnum<'_> {
        let mut string_lits = self.string_lits.borrow_mut();
        match string_lits.get(val) {
            Some(str_ptr) => str_ptr.as_basic_value_enum(),
            None => {
                let str_ptr = self
                    .builder
                    .borrow()
                    .build_global_string_ptr(val.as_str(), "str")
                    .as_pointer_value();
                string_lits.insert(val.clone(), str_ptr);
                str_ptr.as_basic_value_enum()
            }
        }
    }

    fn get_var(&self, sem_var: &semantics::Var) -> Var {
        match self.vars.borrow().iter().rev().find(|var| var.ident == sem_var.ident && var.typ == sem_var.typ) {
            Some(val) => val.clone(),
            None => panic!("Variable was not found in scope, this should not be possible after semantic analysis!"),
        }
    }
}
