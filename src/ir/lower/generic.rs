use codespan_reporting::diagnostic::{Diagnostic, Label};
use hashbrown::HashMap;

use crate::{ir::{TypeId, types::IrType, FunId, IrFun}, Symbol, util::{files::FileId, loc::Span}, ast::{GenericArgs, Stmt}};

use super::{IrLowerer, IntermediateModuleId};

/// Structure containing a list of generic arguments passed to a templated definition
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IntermediateGenericArgs {
    args: Vec<TypeId>,
}

/// Structure containing multiple specializations of a generic templated value
pub struct GenericSpecializations<T, E> {
    /// Name of this template
    name: Symbol,
    /// Parameter names and length
    params: Vec<Symbol>,
    /// The template to specialize with new arguments
    template: E,
    /// Existing specializations of this value
    specs: HashMap<IntermediateGenericArgs, T>,
}


impl<'ctx> IrLowerer<'ctx> {
    /// Retrieve an existing type specialization or create a new one for the given generic type
    pub(super) fn specialize_type(&mut self, module: IntermediateModuleId, file: FileId, span: Span, ty: TypeId, args: &GenericArgs) -> Result<TypeId, Diagnostic<FileId>> {
        let args = IntermediateGenericArgs { args: args
            .args
            .iter()
            .map(|arg| self.resolve_type(arg, module, file, span))
            .collect::<Result<Vec<_>, _>>()?
        };
        match self.generic_types.get(&ty) {
            Some(ref specs) => match specs.specs.get(&args) {
                Some(spec) => Ok(*spec),
                None => {
                    if args.args.len() != specs.params.len() {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "Incorrect number of generic arguments passed to type template {}; expected {}, got {}",
                                self.ctx.typename(ty),
                                specs.params.len(),
                                args.args.len(),
                            ))
                            .with_labels(vec![
                                Label::primary(file, span)
                            ])
                        )
                    }
                    
                    let bindings = args
                        .args
                        .iter()
                        .zip(specs.params.iter())
                        .map(|(arg, param)| (*param, *arg))
                        .collect::<HashMap<_, _>>();

                    self
                        .generic_args
                        .push(bindings);
                    
                    let template = specs.template.clone();
                    let name = specs.name.clone();
                    drop(specs);
                    let specialized = self.resolve_type(&template, module, file, span)?;
                    let specialized = IrType::Alias {
                        name: Symbol::new(format!("{}{}", name, self.ctx.generics(&args.args))),
                        ty: specialized
                    };
                    let specialized = self.ctx.types.insert(specialized);
                    self.generic_types.get_mut(&ty).unwrap().specs.insert(args, specialized); 

                    self
                        .generic_args
                        .pop();

                    Ok(specialized)
                }
            },
            None => Ok(ty),
        }
    }

    /// Retrieve an existing type specialization or create a new one for the given generic type
    pub(super) fn specialize_fn(&mut self, module: IntermediateModuleId, file: FileId, span: Span, fun: FunId, args: &GenericArgs, body: Option<&[Stmt]>) -> Result<FunId, Diagnostic<FileId>> {
        let args = IntermediateGenericArgs { args: args
            .args
            .iter()
            .map(|arg| self.resolve_type(arg, module, file, span))
            .collect::<Result<Vec<_>, _>>()?
        };
        match self.generic_funs.get(&fun) {
            Some(specs) => match specs.specs.get(&args) {
                Some(spec) => Ok(*spec),
                None => {
                    if args.args.len() != specs.params.len() {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "Incorrect number of generic arguments passed to function template {}; expected {}, got {}",
                                self.ctx[fun].name,
                                specs.params.len(),
                                args.args.len(),
                            ))
                            .with_labels(vec![
                                Label::primary(file, span)
                            ])
                        )
                    }
                    
                    let bindings = args
                        .args
                        .iter()
                        .zip(specs.params.iter())
                        .map(|(arg, param)| (*param, *arg))
                        .collect::<HashMap<_, _>>();
                    
                    let template = specs.template.clone();
                    drop(specs);

                    self
                        .generic_args
                        .push(bindings);
                    
                    let ty = self.resolve_fn_type(&template.proto.ty, module, file, span)?;
                    
                    let specialized = self.ctx.funs.insert(IrFun {
                        file,
                        span,
                        name: Symbol::new(format!("{}{}", self.ctx[fun].name, self.ctx.generics(&args.args))),
                        ty_id: self.ctx.types.insert(IrType::Fun(ty.clone())),
                        ty,
                        body: None,
                        flags: template.proto.flags,
                    });
                    
                    let old_bb = self.bb;
                    self.lower_body(module, file, specialized, body.unwrap_or(&template.body))?;
                    self.bb = old_bb;
                    self.generic_funs.get_mut(&fun).unwrap().specs.insert(args, specialized); 

                    self
                        .generic_args
                        .pop();

                    Ok(specialized)
                }
            },
            None => Ok(fun),
        }
    }

    /// Lookup the type bound to this generic argument's name
    pub(super) fn resolve_generic_arg(&self, name: &Symbol) -> Option<TypeId> {
        self
            .generic_args
            .iter()
            .rev()
            .map(|scope| scope.get(name).copied())
            .last()
            .flatten()
    }

}

impl<T, E> GenericSpecializations<T, E> {
    pub fn new(name: Symbol, params: Vec<Symbol>, template: E) -> Self {
        Self {
            name,
            specs: HashMap::new(),
            params,
            template,
        }
    }
}
