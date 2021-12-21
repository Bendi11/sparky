use string_interner::StringInterner;

use crate::{ast::{ParsedModule, DefData, Def, UnresolvedType}, util::{files::{Files, FileId}, loc::Span}, error::{DiagnosticManager, Diagnostic}};
use super::*;


/// A structure responsible for lowering a parsed AST into
/// intermediate representation, resolving all unknown symbols
#[derive(Debug)]
pub struct AstLowerer<'ctx, 'sym, 'files> {
    ctx: &'ctx mut IRContext,
    symbols: &'sym StringInterner,
    /// A map of module names to their IDs in the context
    forward_modules: HashMap<Symbol, ModuleId>,
    /// Structure responsible for outputting emitted diagnostics
    diagnostic: DiagnosticManager<'files>,
}

pub type SemanticResult<T> = Result<T, ()>;

impl<'ctx, 'sym, 'files> AstLowerer<'ctx, 'sym, 'files> {
    /// Create a new lowerer from an ir context
    pub fn new(
        ctx: &'ctx mut IRContext, 
        symbols: &'sym StringInterner, 
        files: &'files Files, 
    ) -> Self {
        Self {
            ctx,
            symbols,
            diagnostic: DiagnosticManager::new(files),
            forward_modules: HashMap::new()
        }
    }
    
    /// Generate code for all passed modules
    pub fn codegen(&mut self, modules: &[ParsedModule]) -> SemanticResult<()> {
        self.forward_modules = self.forward_modules(modules)?;
        self.walk_modules_with(modules, &mut Self::forward_types)?;
        self.walk_modules_with(modules, &mut Self::resolve_types)?;
        println!("{:#?}", self.ctx);
        Ok(())
    }
        
    
    /// Generate forward definitions of all modules in the IR context and return a map of module
    /// names to more module IDs
    pub fn forward_modules(&mut self, modules: &[ParsedModule]) -> SemanticResult<HashMap<Symbol, ModuleId>> {
        fn forward_modules_for(ctx: &mut IRContext, list: &HashMap<Symbol, ParsedModule>) -> SemanticResult<HashMap<Symbol, ModuleId>> {
            let mut submods = HashMap::new();
            for submod in list.values() {
                let irmod_id = ctx.new_module(submod.name);
                ctx.modules[irmod_id].children = forward_modules_for(ctx, &submod.children)?;
                submods.insert(submod.name, irmod_id);
            }
            Ok(submods)
        }
        let mut modules_map = HashMap::new();
        for module in modules.iter() {
            let irmod_id = self.ctx.new_module(module.name);
            let children = forward_modules_for(&mut self.ctx, &module.children)?;
            self.ctx.modules[irmod_id].children = children;

            modules_map.insert(module.name, irmod_id);
        }
        Ok(modules_map)
    }
    
    /// Create Type entries for all type definitions in a module
    fn forward_types(&mut self, (parsed, id): (&ParsedModule, ModuleId)) -> SemanticResult<()> {
        //Keep a map of recorded definitions to check for duplicate names
        let mut remembered_defs: HashMap::<Symbol, Def>  = HashMap::new();
        
        for def in parsed.defs.values() {
            match def.data {
                DefData::StructDef { name, fields: _ }
                | DefData::EnumDef { name, variants: _ }
                | DefData::AliasDef { name, aliased: _ } => {
                    //Ensure that two type definitions don't have colliding names
                    if let Some(ref other_def) = remembered_defs.get(&name) {
                        self.diagnostic.emit(
                            Diagnostic::error(
                                format!("In module {}: two type definitions with the same name", self.symbol(parsed.name)), 
                                parsed.file
                            )
                            .with_span(def.span)
                            .with_span(other_def.span)
                        );
                        return Err(())
                    }
                    remembered_defs.insert(name, def.clone());

                    self.ctx.modules[id].typedefs.insert(name, self.ctx.invalid_id);
                },
                _ => (),
            }
        }
        Ok(())

    }
    
    /// Resolve all previously forward declared types with definitions
    fn resolve_types(&mut self, (parsed, id): (&ParsedModule, ModuleId)) -> SemanticResult<()> {
        for def in parsed.defs.values() {
            match &def.data {
                DefData::StructDef {
                    name,
                    fields
                } => {
                    let struct_id = *self.ctx.modules[id].typedefs.get(name).expect("Forward declarations created for all types");
                    let struct_ty = TypeData::Struct {
                        fields: fields
                            .iter()
                            .map(|(name, ty)| match self.resolve_type(def.span, parsed.file, ty, id) {
                                Ok(ty) => Ok((*name, ty)),
                                Err(e) => Err(e)
                            })
                            .collect::<Result<HashMap<_, _>, _>>()?
                    };
                    self.ctx.types[struct_id].data = struct_ty;
                },
                DefData::EnumDef {
                    name,
                    variants
                } => {
                    let enum_id = *self
                        .ctx
                        .modules[id]
                        .typedefs
                        .get(name)
                        .expect("Forward declarations already declared");
                    let enum_ty = TypeData::Enum {
                        variants: variants
                            .iter()
                            .map(|variant| self.resolve_type(def.span, parsed.file, variant, id))
                            .collect::<Result<Vec<_>, _>>()?
                    };
                    self.ctx.types[enum_id].data = enum_ty;
                },
                DefData::AliasDef {
                    name,
                    aliased
                } => {
                    let alias_id = *self
                        .ctx
                        .modules[id]
                        .typedefs
                        .get(name)
                        .expect("Forward declarations already declared");
                    let alias_ty = TypeData::Alias(self.resolve_type(def.span, parsed.file, aliased, id)?);
                    self.ctx.types[alias_id].data = alias_ty;
                }
                _ => ()
            }
        }
        Ok(())
    }


    /// Walk a module and any submodules 
    fn walk_module_with<R>(
        &mut self, 
        module: &ParsedModule, 
        f: &mut dyn FnMut(&mut Self, (&ParsedModule, ModuleId)) -> SemanticResult<R>
    ) -> SemanticResult<R>{
        let id = self.forward_modules[&module.name];
        for submodule in module.children.values() {
           self.walk_module_with(submodule, f)?; 
        }
        f(self, (module, id))
    }

    /// Walk all modules using the specified function
    fn walk_modules_with<R>(
        &mut self,
        modules: &[ParsedModule],
        f: &mut dyn FnMut(&mut Self, (&ParsedModule, ModuleId)) -> SemanticResult<R>
    ) -> SemanticResult<R> {
        for module in modules.iter().skip(1) {
            self.walk_module_with(module, f)?;
        }
        self.walk_module_with(&modules[0], f)
    }

    fn symbol(&self, sym: Symbol) -> &str {
        self.symbols.resolve(sym).unwrap()
    }
    
    /// Resolve a type in the context of the given module
    fn resolve_type(&mut self, span: Span, file_id: FileId, ty: &UnresolvedType, module: ModuleId) -> SemanticResult<TypeId> {
        Ok(match ty {
            UnresolvedType::Integer { width, signed } => match signed {
                true => self.ctx.i_ids[*width as u8 as usize],
                false => self.ctx.u_ids[*width as u8 as usize],
            },
            UnresolvedType::Bool => self.ctx.bool_id,
            UnresolvedType::Unit => self.ctx.unit_id,
            UnresolvedType::Fun(unresolved_fun_ty) => {
                let fun_ty_id = self.ctx.new_type();
                let return_ty = self.resolve_type(span, file_id, &unresolved_fun_ty.return_ty, module)?;
                let args = unresolved_fun_ty.arg_tys.iter()
                        .map(|ty| self.resolve_type(span, file_id, ty, module))
                        .collect::<Result<Vec<_>, _>>()?;

                let fun_ty = &mut self.ctx.types[fun_ty_id];
                fun_ty.data = TypeData::Fun {
                    return_ty, 
                    args,
                };
                fun_ty_id
            },
            UnresolvedType::Float { doublewide } => if *doublewide { self.ctx.f64_id } else { self.ctx.f32_id },
            UnresolvedType::Array { elements, len } => {
                let array_type_id = self.ctx.new_type();
                let elements = self.resolve_type(span, file_id, elements, module)?;
                let array_ty = &mut self.ctx.types[array_type_id];
                array_ty.data = TypeData::Array {
                    elements,                   
                    len: *len,
                };
                array_type_id
            },
            UnresolvedType::Tuple { elements } => {
                let tuple_type_id = self.ctx.new_type();
                let elements =  elements.iter()
                        .map(|ty| self.resolve_type(span, file_id, ty, module))
                        .collect::<Result<Vec<_>, _>>()?;

                let tuple_type = &mut self.ctx.types[tuple_type_id];
                tuple_type.data = TypeData::Tuple { 
                    elements,
                };
                tuple_type_id
            },
            UnresolvedType::Pointer(to) => {
                let pointer_ty_id = self.ctx.new_type();
                let pointee_ty = self.resolve_type(span, file_id, &*to, module)?;
                let pointer_ty = &mut self.ctx.types[pointer_ty_id];
                pointer_ty.data = TypeData::Pointer(pointee_ty);
                pointer_ty_id
            },
            UnresolvedType::UserDefined { name } => {
                match self.ctx.get_type(module, name) {
                    Some(id) => id,
                    None => {
                        self.diagnostic.emit(Diagnostic::error(
                                format!("Unknown type name '{}'", self.display_path(name)),
                                file_id
                            )
                            .with_span(span)
                        );
                        return Err(())
                    }
                }
            }
        })
    }

    fn display_path(&self, path: &SymbolPath) -> String {
        let mut display = String::new();
        for (i, part) in path.iter().enumerate() {
            display.push_str(self.symbol(part));
            if i != path.len() - 1 {
                display.push(':');
            }
        }
        display
    }

}
