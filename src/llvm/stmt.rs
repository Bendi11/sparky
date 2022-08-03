use std::convert::TryFrom;

use inkwell::values::{BasicValueEnum, AnyValueEnum, CallableValue};

use crate::ir::{IrStmt, IrStmtKind, IrContext, BBId};

use super::{LLVMCodeGenerator, LLVMCodeGeneratorState};


impl<'files, 'llvm> LLVMCodeGeneratorState<'files, 'llvm> {
    /// Translate IR to LLVM bytecode for a single basic block
    pub fn gen_bb(&mut self, irctx: &IrContext, bb: BBId) { 
        for stmt in irctx[bb].stmts.iter() {
            self.gen_stmt(irctx, stmt);
        }
    }

    /// Translate one IR statement to LLVM bytecode instructions
    pub fn gen_stmt(&mut self, irctx: &IrContext, stmt: &IrStmt) {
        match &stmt.kind {
            IrStmtKind::VarLive(v) => {
                let var = &irctx[*v];
                let pv = self.build.build_alloca(*self.llvm_types.get_secondary(var.ty), var.name.as_str());
                *self.llvm_vars.get_secondary_mut(*v) = Some(pv);
            },
            IrStmtKind::Store { var, val } => {
                let alloca = self.llvm_vars.get_secondary(*var).unwrap();
                let val = self.gen_expr(irctx, val);
                self.build.build_store(alloca, val);
            },
            IrStmtKind::Write { ptr, val } => {
                let ptr = self.gen_lval(irctx, ptr);
                let val = self.gen_expr(irctx, val);
                self.build.build_store(ptr, val);
            },
            IrStmtKind::Call { fun, args } => {
                let fun = *self.llvm_funs.get_secondary(*fun);
                let args = args
                    .iter()
                    .map(|arg| self.gen_expr(irctx, arg).into())
                    .collect::<Vec<_>>();
                self.build.build_call(fun, &args, "call");
            },
        }
    }
}
