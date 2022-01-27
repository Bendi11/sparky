//! Generating LLVM IR from a parsed and type lowered AST

pub mod astgen;

use std::convert::TryFrom;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use hashbrown::HashMap;
use inkwell::{
    AddressSpace, 
    builder::Builder,
    context::Context,
    module::{Module, Linkage},
    targets::{TargetData, TargetMachine, Target, InitializationConfig, RelocMode, CodeModel},
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType as InkwellFunctionType, PointerType},
    values::{FunctionValue, BasicValueEnum, PointerValue}, OptimizationLevel, basic_block::BasicBlock
};
use quickscope::ScopeMap;

use crate::{Symbol, ast::{IntegerWidth, SymbolPath}, codegen::ir::{ModId, SparkCtx, TypeId, SparkDef, TypeData, FunctionType, FunId}, error::DiagnosticManager, util::{files::{FileId, Files}, loc::Span}};

/// A type representing all types that can be defined in the global scope 
/// map of the code generator
#[derive(Clone, Copy)]
enum ScopeDef<'ctx> {
    Value(TypeId, PointerValue<'ctx>),
    Def(SparkDef),
}

/// Structure that generates LLVM IR modules from a parsed and 
/// type lowered AST module
pub struct LlvmCodeGenerator<'ctx, 'files> {
    pub ctx: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub spark: SparkCtx,
    pub diags: DiagnosticManager<'files>,
    llvm_funs: HashMap<FunId, FunctionValue<'ctx>>,
    target: TargetData,
    current_scope: ScopeMap<Symbol, ScopeDef<'ctx>>,
    current_fun: Option<(FunctionValue<'ctx>, FunId)>,
    break_data: Option<BreakData<'ctx>>,
}

/// Data needed to use a phi / break / continue statement
#[derive(Clone, Copy)]
struct BreakData<'ctx> {
    pub return_to_bb: BasicBlock<'ctx>,
    pub phi_data: Option<PhiData<'ctx>>,
}

/// Data needed specifcally for a phi statement
#[derive(Clone, Copy)]
struct PhiData<'ctx> {
    pub alloca: PointerValue<'ctx>,
    pub phi_ty: TypeId,
}

impl<'ctx, 'files> LlvmCodeGenerator<'ctx, 'files> {
    /// Create a new code generator from an LLVM context
    pub fn new(spark: SparkCtx, ctx: &'ctx Context, files: &'files Files) -> Self {
        Target::initialize_native(&InitializationConfig::default()).expect("LLVM: failed to initialize native compilation target");

        Self {
            current_scope: ScopeMap::new(),
            current_fun: None,
            builder: ctx.create_builder(),
            ctx,
            spark,
            diags: DiagnosticManager::new(files),
            llvm_funs: HashMap::new(),
            break_data: None,
            target: Target::from_triple(&TargetMachine::get_default_triple()).unwrap().create_target_machine(
                &TargetMachine::get_default_triple(),
                TargetMachine::get_host_cpu_name().to_str().unwrap(),
                TargetMachine::get_host_cpu_features().to_str().unwrap(),
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Medium
            ).unwrap().get_target_data(),
        }
    }
    
    /// Find a name in the current scope
    fn find_in_scope(&self,
        file: FileId, 
        module: ModId, 
        span: Span, 
        path: &SymbolPath
    ) -> Result<ScopeDef<'ctx>, Diagnostic<FileId>> {
        let mut iter = path.iter();

        let first = iter.next().unwrap();

        match self.current_scope.get(&first) {
            Some(def) => if iter.len() == 0 {
                Ok(*def)
            } else {
                match *def {
                    ScopeDef::Def(SparkDef::ModDef(submod)) => {
                        self.spark.get_def_impl(submod, iter)
                            .map(|d| ScopeDef::Def(d))
                            .map_err(|name| Diagnostic::error()
                                .with_message(format!("'{}' not found in current scope", name))
                                .with_labels(vec![Label::primary(file, span)])
                            )
                    },
                    _ => Err(Diagnostic::error()
                        .with_message(format!(
                                "Cannot access '{}' of non-module definition",
                                iter.map(|s| s.as_str().to_owned()).collect::<Vec<_>>().join(":")
                            )
                        )
                        .with_labels(vec![Label::primary(file, span)])
                    )
                } 
            },
            None => Err(Diagnostic::error()
                .with_message(format!("Symbol '{}' not found in the current scope", first))
                .with_labels(vec![Label::primary(file, span)])
            )
        }
    }
    
    /// Codegen LLVM IR from a type-lowered module
    pub fn codegen_module(&mut self, module: ModId) -> Module<'ctx> {
        let mut llvm_mod = self.ctx.create_module(self.spark[module].name.as_str());

        self.forward_funs(&mut llvm_mod, module);
        
        let defs = self.spark[module].defs.clone();

        self.current_scope.push_layer();

        for (name, def) in defs.iter() {
            self.current_scope.define(name.clone(), ScopeDef::Def(*def));
        }
        
        let defs = self.spark[module].defs.clone();
        for (name, def) in defs.iter() {

            if let SparkDef::FunDef(file, fun) = def {
                if let Some(ref body) = self.spark[*fun].body {
                    let llvm_fun = *self.llvm_funs.get(fun).unwrap();
                    let entry = self.ctx.append_basic_block(llvm_fun, "entry_bb");
                    self.builder.position_at_end(entry);

                    self.current_fun = Some((llvm_fun, *fun));
                    for stmt in body.clone() {
                        if let Err(e) = self.gen_stmt(*file, module, stmt) {
                            self.diags.emit(e.with_notes(vec![format!("In function {}", name)]));
                        }
                    }
                }
            }
        }

        self.current_scope.pop_layer();

        llvm_mod
    }
    
    /// Generate code for all function prototypes
    fn forward_funs(&mut self, llvm: &mut Module<'ctx>, module: ModId) {
        let defs = self.spark[module].defs.clone();

        for fun_id in defs
            .iter()
            .filter_map(|(_, def)| if let SparkDef::FunDef(_, id) = def { Some(*id) } else { None }) {
            let fun = self.spark[fun_id].clone();
            let llvm_fun_ty = self.gen_fun_ty(&fun.ty);
            let llvm_fun = llvm.add_function(fun.name.as_str(), llvm_fun_ty, Some(Linkage::External));
            self.llvm_funs.insert(fun_id, llvm_fun);
        }
    }

    /// Create an LLVM type from a type ID
    fn llvm_ty(&mut self, id: TypeId) -> AnyTypeEnum<'ctx> {
        match self.spark[id].data.clone() {
            TypeData::Integer { signed: _, width } => match width {
                IntegerWidth::Eight => self.ctx.i8_type().into(),
                IntegerWidth::Sixteen => self.ctx.i16_type().into(),
                IntegerWidth::ThirtyTwo => self.ctx.i32_type().into(),
                IntegerWidth::SixtyFour => self.ctx.i64_type().into(),
            },
            TypeData::Bool => self.ctx.bool_type().into(),
            TypeData::Tuple(elems) => {
                let elems = elems
                    .iter()
                    .map(|id| BasicTypeEnum::try_from(self.llvm_ty(*id)).unwrap())
                    .collect::<Vec<_>>();
                self.ctx.struct_type(&elems, false).into()
            },
            TypeData::Struct { fields } => {
                let fields = fields
                    .iter()
                    .map(|(id, _)| BasicTypeEnum::try_from(self.llvm_ty(*id)).unwrap())
                    .collect::<Vec<_>>();
                self.ctx.struct_type(&fields, false).into()
            },
            TypeData::Alias(_, id) => self.llvm_ty(id),
            TypeData::Pointer(id) => {
                let pointee = self.llvm_ty(id);

                match BasicTypeEnum::try_from(pointee) {
                    Ok(b) => b
                        .ptr_type(AddressSpace::Generic)
                        .into(),
                    Err(_) => unimplemented!() 
                }
            },
            TypeData::Array { element, len } => 
                BasicTypeEnum::try_from(self.llvm_ty(element))
                    .unwrap()
                    .array_type(len as u32)
                    .into(),
            TypeData::Unit => self.ctx.void_type().into(),
            TypeData::Invalid => unreachable!(),
            TypeData::Float { doublewide } => match doublewide {
                true => self.ctx.f64_type().into(),
                false => self.ctx.f32_type().into(),
            },
            TypeData::Function(ty) => self.gen_fun_ty(&ty).ptr_type(AddressSpace::Generic).into(),
            TypeData::Enum { parts } => {
                let parts = parts
                    .iter()
                    .map(|ty| self.llvm_ty(*ty))
                    .collect::<Vec<_>>();
                let max_size = parts.iter().map(|ty| {
                        if let Ok(ty) = BasicTypeEnum::try_from(*ty) {
                            ty.size_of().map(|i| i.get_zero_extended_constant().unwrap() as u32).unwrap_or(0)
                        } else {
                            0
                        }
                    })
                    .max()
                    .unwrap();

                let field_types = &[self.ctx.i8_type().into(), self.ctx.i8_type().array_type(max_size).into()];

                self.ctx.struct_type(field_types, true).into() 
            }
        }

    }
    
    /// Create an LLVM function type from a spark IR function type
    fn gen_fun_ty(&mut self, ty: &FunctionType) -> InkwellFunctionType<'ctx> {
        let return_ty = self.llvm_ty(ty.return_ty);
        let args = ty
            .args
            .iter()
            .map(|ty| BasicTypeEnum::try_from(self.llvm_ty(*ty)).unwrap().into())
            .collect::<Vec<_>>();
        match return_ty {
            AnyTypeEnum::VoidType(return_ty) => 
                return_ty.fn_type(&args, false),
            _ => BasicTypeEnum::try_from(return_ty)
                .unwrap()
                .fn_type(&args, false)
        }

    }
}


