use std::convert::{TryFrom, TryInto};

use hashbrown::HashMap;
use inkwell::{values::{BasicValueEnum, PointerValue, ArrayValue, CallableValue}, types::{BasicType, BasicTypeEnum}};

use crate::{ir::{IrContext, value::{IrExpr, IrExprKind, IrLiteral}, types::IrType}, parse::token::Op};

use super::{LLVMCodeGeneratorState, LLVMCodeGenerator};



impl<'files, 'llvm> LLVMCodeGeneratorState<'files, 'llvm> {
    fn array_vals<T: TryFrom<BasicValueEnum<'llvm>>>(&mut self, irctx: &IrContext, vals: &[IrExpr]) -> Vec<T> 
    where 
        <T as TryFrom<BasicValueEnum<'llvm>>>::Error: std::fmt::Debug 
    {
        vals
            .iter()
            .map(|val| self.gen_expr(irctx, val).try_into().unwrap())
            .collect::<Vec<_>>()
    }

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
                    let elem = if let IrType::Array(ty, _) = &irctx[ty] {
                        *ty
                    } else { unreachable!() };

                    

                    match self
                        .llvm_types
                        .get_secondary(elem) {
                            BasicTypeEnum::ArrayType(ty) => ty.const_array(&self.array_vals(irctx, vals)),
                            BasicTypeEnum::PointerType(ty) => ty.const_array(&self.array_vals(irctx, vals)),
                            BasicTypeEnum::StructType(ty) => ty.const_array(&self.array_vals(irctx, vals)),
                            BasicTypeEnum::FloatType(ty) => ty.const_array(&self.array_vals(irctx, vals)),
                            BasicTypeEnum::IntType(ty) => ty.const_array(&self.array_vals(irctx, vals)),
                            BasicTypeEnum::VectorType(ty) => ty.const_array(&self.array_vals(irctx, vals)),
                        }.into()
                        
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
            IrExprKind::Cast(expr, ty) => {
                let llvm_expr = self.gen_expr(irctx, expr);
                if irctx.unwrap_alias(expr.ty) == irctx.unwrap_alias(*ty) {
                    llvm_expr
                } else {
                    todo!("{} is not the same as {}", irctx.typename(expr.ty), irctx.typename(*ty))
                }
            },
            IrExprKind::Unary(op, expr) => match op {
                Op::AND => self.gen_lval(irctx, expr).into(),
                _ => todo!(),
            }
            _ => todo!("{:?} is not yet implemented", expr)
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

                unsafe{ self
                    .build
                    .build_in_bounds_gep(obj, &[self.ctx.i64_type().const_zero(), self.ctx.i32_type().const_int(*field as u64, false)], "struct_gep")
                    //.unwrap()
                }
            },
            IrExprKind::Index(arr, elem) => {
                let arr = self.gen_lval(irctx, arr);
                let elem = self.gen_expr(irctx, elem).into_int_value();
                unsafe {
                    self
                        .build
                        .build_in_bounds_gep(
                            arr,
                            &[self.ctx.i32_type().const_zero(), elem],
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
