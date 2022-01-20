use crate::{error::{DiagnosticManager, Diagnostic}, util::{files::Files, loc::Span}, ast::{ParsedModule, Def, DefData, FunProto, UnresolvedType, IntegerWidth}};

use super::ir::{SparkCtx, ModId, FunId, TypeId, FunctionType, TypeData, SparkDef};

/// Structure for lowering a parsed AST's types
pub struct Lowerer<'ctx, 'files> {
    ctx: &'ctx mut SparkCtx,
    diags: DiagnosticManager<'files>,
}

impl<'ctx, 'files> Lowerer<'ctx, 'files> {
    /// Create a new AST lowerer
    pub fn new(ctx: &'ctx mut SparkCtx, files: &'files Files) -> Self {
        Self {
            ctx,
            diags: DiagnosticManager::new(files),
        }
    }
    
    /// Lower a parsed module's definitions and return an ID for the lowered
    /// module
    pub fn lower_module(&mut self, parsed: &ParsedModule) -> ModId {
        let module_id = self.ctx.new_module(parsed.name);
        let module = &mut self.ctx[module_id];
        for def in parsed.defs.iter().map(|(_, v)| v) {
            match def {
                DefData::FunDef(proto, body) => {
                    
                }
            }
        }
    }
    
    /// Lower a single function prototype to a function with no body
    fn lower_funproto(&mut self, proto: &FunProto<UnresolvedType>) -> FunId {

    }
    
    /// Lower a type either by resolving the path to the type or 
    /// converting an integral type
    fn lower_type(&mut self, module: ModId, span: Option<Span>, ty: &UnresolvedType) -> TypeId {
        match ty {
            UnresolvedType::Integer { width, signed } => match signed {
                true => match width {
                    IntegerWidth::Eight => SparkCtx::I8,
                    IntegerWidth::Sixteen => SparkCtx::I16,
                    IntegerWidth::ThirtyTwo => SparkCtx::I32,
                    IntegerWidth::SixtyFour => SparkCtx::I64
                },
                false => match width {
                    IntegerWidth::Eight => SparkCtx::U8,
                    IntegerWidth::Sixteen => SparkCtx::U16,
                    IntegerWidth::ThirtyTwo => SparkCtx::U32,
                    IntegerWidth::SixtyFour => SparkCtx::U64 
                }
            },
            UnresolvedType::Float { doublewide } => match doublewide {
                true => SparkCtx::F64,
                false => SparkCtx::F32,
            },
            UnresolvedType::Unit => SparkCtx::UNIT,
            UnresolvedType::Bool => SparkCtx::BOOL,
            UnresolvedType::Fun(ty) => self.ctx.new_type(TypeData::Function(FunctionType {
                return_ty: self.lower_type(module, span, &ty.return_ty),
                args: ty.arg_tys.iter().map(|ty| self.lower_type(module, span, ty)).collect()
            })),
            UnresolvedType::Pointer(ty) => self.ctx.new_type(TypeData::Pointer(self.lower_type(module, span, ty))),
            UnresolvedType::Array { elements, len } => self.ctx.new_type(TypeData::Array {
                element: self.lower_type(module, span, elements),
                len: *len,
            }),
            UnresolvedType::Tuple { elements } => self.ctx.new_type(TypeData::Tuple(elements.iter().map(|ty| self.lower_type(module, span, ty)).collect())),
            UnresolvedType::UserDefined { name } => match self.ctx.get_def(module, name) {
                Ok(SparkDef::TypeDef(type_id)) => type_id,
                _ => {
                    self.diags.emit({
                        let mut diag = Diagnostic::error(format!("type {} not found", name), self.ctx[module].file);
                        if let Some(span) = span {
                            diag.with_span(span)
                        } else {
                            diag
                        }
                    });
                    panic!()
                }
            }
        }
    }
}
