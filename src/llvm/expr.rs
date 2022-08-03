use std::convert::TryFrom;

use hashbrown::HashMap;
use inkwell::{values::{BasicValueEnum, PointerValue, ArrayValue, CallableValue}, types::BasicType};

use crate::ir::{IrContext, value::{IrExpr, IrExprKind, IrLiteral}, types::IrType};

use super::{LLVMCodeGeneratorState, LLVMCodeGenerator};



impl<'files, 'llvm> LLVMCodeGeneratorState<'files, 'llvm> {
    ///Generate LLVM bytecode for a single IR expression
    pub fn gen_expr(&mut self, irctx: &IrContext, expr: &IrExpr) -> BasicValueEnum<'llvm> {
        match &expr.kind {
            IrExprKind::Var(v) => {
                let alloca = self.gen_lval(irctx, expr);
                self.build.build_load(alloca, "var_load")
            },
            IrExprKind::Lit(lit) => match lit {
                IrLiteral::Integer(v, ty) => self
                    .ctx
                    .i64_type()
                    .const_int(v.val, v.sign)
                    .const_cast(LLVMCodeGenerator::gen_inttype(&self.ctx, ty), ty.signed)
                    .into(),
                IrLiteral::Float(f, ty) => self
                    .ctx
                    .f64_type()
                    .const_float(*f)
                    .const_cast(if ty.doublewide { self.ctx.f64_type() } else { self.ctx.f32_type() })
                    .into(),
                IrLiteral::Bool(b) => self
                    .ctx
                    .bool_type()
                    .const_int(if *b { 1 } else { 0 }, false)
                    .into(),
                IrLiteral::Array(vals) => {
                    let ty = expr.ty;
                    let vals = vals
                        .iter()
                        .map(|val| ArrayValue::try_from(self.gen_expr(irctx, val)).unwrap())
                        .collect::<Vec<_>>();
                    self
                        .llvm_types
                        .get_secondary(ty)
                        .into_array_type()
                        .const_array(&vals)
                        .into()
                },
                IrLiteral::Unit => self.ctx.i8_type().const_int(0, false).into(),
                IrLiteral::Struct(s) => {
                    let irty = if let IrType::Struct(s_ty) = &irctx[expr.ty] {
                        s_ty
                    } else {
                        unreachable!()
                    };

                    let ty = self.llvm_types.get_secondary(expr.ty).into_struct_type();
                    let mut fields = HashMap::new();
                    for (name, field) in s.iter() {
                        let idx = irty.field_idx(name).unwrap();
                        fields.insert(idx, self.gen_expr(irctx, field));
                    }

                    let mut field_vec = vec![];
                    for i in 0..s.len() {
                        field_vec.push(*fields.get(&i).unwrap());
                    }

                    ty.const_named_struct(&field_vec).into()
                },
                IrLiteral::String(s) => {
                    self
                        .build
                        .build_global_string_ptr(s.as_str(), "strlit")
                        .as_pointer_value()
                        .into()
                        
                }
            }
            IrExprKind::Call(fun, args) => {
                let fun = self.gen_expr(irctx, fun).into_pointer_value();
                let callable = CallableValue::try_from(fun).unwrap();
                let args = args
                    .iter()
                    .map(|arg| self.gen_expr(irctx, arg).into())
                    .collect::<Vec<_>>();

                self
                    .build
                    .build_call(callable, &args, "call")
                    .try_as_basic_value()
                    .left()
                    .unwrap_or(self.ctx.i8_type().const_int(0, false).into())
            },
            IrExprKind::Fun(..) | IrExprKind::Member(..) | IrExprKind::Index(..) => self.gen_lval(irctx, expr).into(),
            _ => todo!()
        }
    }
    
    /// Generate code for an Lvalue
    pub fn gen_lval(&mut self, irctx: &IrContext, expr: &IrExpr) -> PointerValue<'llvm> {
        match &expr.kind {
            IrExprKind::Var(v) => self.llvm_vars.get_secondary(*v).unwrap(),
            IrExprKind::Fun(f) => self
                .llvm_funs
                .get_secondary(*f)
                .as_global_value()
                .as_pointer_value(),
            IrExprKind::Member(obj, field) => {
                let obj = self.gen_lval(irctx, obj);

                self
                    .build
                    .build_struct_gep(obj, *field as u32, "struct_gep")
                    .unwrap()
            },
            IrExprKind::Index(arr, elem) => {
                let arr = self.gen_lval(irctx, arr);
                let elem = self.gen_expr(irctx, elem).into_int_value();
                unsafe {
                    self
                        .build
                        .build_in_bounds_gep(
                            arr,
                            &[self.ctx.i8_type().const_zero(), elem],
                            "arr_index"
                        )
                        .into()
                }
            },
            _ => {
                let alloca = self.build.build_alloca(*self.llvm_types.get_secondary(expr.ty), "lval_alloca");
                let val = self.gen_expr(irctx, expr);
                self.build.build_store(alloca, val);
                alloca.into()
            }
        }
    }
}
