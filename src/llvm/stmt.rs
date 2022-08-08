use std::convert::TryFrom;

use inkwell::values::{AnyValueEnum, BasicValueEnum, CallableValue, FunctionValue};

use crate::ir::{types::IrType, BBId, IrContext, IrStmt, IrStmtKind, IrTerminator};

use super::{LLVMCodeGenerator, LLVMCodeGeneratorState};

impl<'files, 'llvm> LLVMCodeGeneratorState<'files, 'llvm> {
    /// Translate IR to LLVM bytecode for a single basic block
    pub fn gen_bb(&mut self, irctx: &IrContext, bb: BBId, fun: FunctionValue<'llvm>) {
        let llvm_bb = {
            let Self { llvm_bbs, ctx, .. } = self;
            *llvm_bbs
                .entry(bb)
                .or_insert_with(|| ctx.append_basic_block(fun, "bb"))
        };

        self.build.position_at_end(llvm_bb);
        for stmt in irctx[bb].stmts.iter() {
            self.gen_stmt(irctx, stmt);
        }

        match &irctx[bb].terminator {
            IrTerminator::Return(v) => {
                if v.ty != IrContext::UNIT {
                    let return_val = self.gen_expr(irctx, &v);
                    self.build.build_return(Some(&return_val));
                } else {
                    self.build.build_return(None);
                }
            }
            IrTerminator::Jmp(bb) => match self.llvm_bbs.get(bb) {
                Some(new_bb) => {
                    self.build.build_unconditional_branch(*new_bb);
                }
                None => {
                    let new_bb = self.ctx.append_basic_block(fun, "bb");
                    self.llvm_bbs.insert(*bb, new_bb);
                    self.build.build_unconditional_branch(new_bb);
                    self.gen_bb(irctx, *bb, fun);
                }
            },
            IrTerminator::JmpIf {
                condition,
                if_true,
                if_false,
            } => {
                let if_true_llvm = self.ctx.append_basic_block(fun, "if_t");
                self.llvm_bbs.insert(*if_true, if_true_llvm);
                let if_false_llvm = self.ctx.append_basic_block(fun, "if_f");
                self.llvm_bbs.insert(*if_false, if_false_llvm);
                let condition = self.gen_expr(irctx, condition).into_int_value();
                self.build
                    .build_conditional_branch(condition, if_true_llvm, if_false_llvm);
                self.gen_bb(irctx, *if_true, fun);
                self.gen_bb(irctx, *if_false, fun);
            }
            IrTerminator::JmpMatch {
                variant,
                discriminants,
                default_jmp,
            } => {
                let sum_ty = if let IrType::Sum(sum) = &irctx[irctx.unwrap_alias(variant.ty)] {
                    sum
                } else {
                    unreachable!("{}", irctx.typename(variant.ty))
                };
                let variant = self.gen_lval(irctx, variant);
                let discrim_ptr = self
                    .build
                    .build_struct_gep(variant, 0, "sum_discrim_ptr")
                    .unwrap();
                let discrim = self
                    .build
                    .build_load(discrim_ptr, "sum_discrim")
                    .into_int_value();

                let after_bb = self.ctx.append_basic_block(fun, "match_default");
                self.llvm_bbs.insert(*default_jmp, after_bb);
                self.gen_bb(irctx, *default_jmp, fun);

                let discriminants = discriminants
                    .iter()
                    .map(|(ty, bb)| {
                        let idx = sum_ty
                            .iter()
                            .enumerate()
                            .find_map(|(idx, variant)| if ty == variant { Some(idx) } else { None })
                            .unwrap();
                        let arm = self.ctx.append_basic_block(fun, "matcharm");
                        self.llvm_bbs.insert(*bb, arm);
                        self.gen_bb(irctx, *bb, fun);

                        (self.ctx.i8_type().const_int(idx as u64, false), arm)
                    })
                    .collect::<Vec<_>>();

                self.build.position_at_end(llvm_bb);
                self.build.build_switch(discrim, after_bb, &discriminants);
                self.build.position_at_end(after_bb);
            }
            IrTerminator::Invalid => panic!("Invalid BB terminator: {}", bb),
        }
    }

    /// Translate one IR statement to LLVM bytecode instructions
    pub fn gen_stmt(&mut self, irctx: &IrContext, stmt: &IrStmt) {
        match &stmt.kind {
            IrStmtKind::VarLive(v) => {
                let var = &irctx[*v];
                let pv = self
                    .build
                    .build_alloca(*self.llvm_types.get_secondary(var.ty), var.name.as_str());
                *self.llvm_vars.get_secondary_mut(*v) = Some(pv);
            }
            IrStmtKind::Store { var, val } => {
                let alloca = self.llvm_vars.get_secondary(*var).unwrap();
                let val = self.gen_expr(irctx, val);
                self.build.build_store(alloca, val);
            }
            IrStmtKind::Write { ptr, val } => {
                let ptr = self.gen_lval(irctx, ptr);
                let val = self.gen_expr(irctx, val);
                self.build.build_store(ptr, val);
            }
            IrStmtKind::Call { fun, args } => {
                let fun = *self.llvm_funs.get_secondary(*fun);
                let args = args
                    .iter()
                    .map(|arg| self.gen_expr(irctx, arg).into())
                    .collect::<Vec<_>>();
                self.build.build_call(fun, &args, "call");
            },
            IrStmtKind::Exec(expr) => {
                self.gen_expr(irctx, expr);
            },
        }
    }
}
