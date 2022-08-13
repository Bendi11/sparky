use std::{collections::BTreeSet, fmt::Display};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use hashbrown::HashMap;

use crate::{ir::{TypeId, types::IrType, FunId, IrFun}, Symbol, util::{files::FileId, loc::Span}, ast::{Stmt, UnresolvedGenericArgs}};

use super::{IrLowerer, IntermediateModuleId};

/// Structure containing a list of generic arguments passed to a templated definition
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericArgs {
    args: Vec<TypeId>,
}

/// A bound that can be placed on a generic type parameter
#[derive(Clone, PartialEq, Eq)]
pub enum GenericBound {
    /// Type must be equal to the given type
    Is(TypeId),
    /// There is no bound on the given type
    Any,
}

/// A collection of [IntermediateGenericBound]s that limit the parameters of a generic value
pub struct GenericTemplate<T> {
    /// The bounds for this template to be applied
    params: Vec<GenericBound>,
    template: T,
}

/// Structure containing multiple specializations of a generic templated value
pub struct GenericSpecializations<Template, Value> {
    /// Name of this template
    pub name: Symbol,
    /// Parameter names and length
    pub params: Vec<Symbol>,
    /// Existing templates for this value
    pub(super) templates: BTreeSet<GenericTemplate<Template>>,
    /// Existing specializations for this generic
    pub(super) specs: HashMap<GenericArgs, Value>,
}


impl<'ctx> IrLowerer<'ctx> {
    /// Retrieve an existing type specialization or create a new one for the given generic type
    pub(super) fn specialize_type(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        span: Span,
        ty: TypeId,
        args: &UnresolvedGenericArgs
    ) -> Result<TypeId, Diagnostic<FileId>> {
        let args = GenericArgs { args: args
            .args
            .iter()
            .map(|arg| self.resolve_type(arg, module, file, span))
            .collect::<Result<Vec<_>, _>>()?
        };
        match self.generic_types.get(&ty) {
            Some(ref specs) => match specs.specs.get(&args) {
                Some(spec) => Ok(*spec),
                None => match specs.get(&args) {
                    Some(template) => {
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
                        
                        let name = specs.name.clone();
                        let template = template.template.clone();
                        drop(specs);
                        let specialized = self.resolve_type(&template, module, file, span)?;
                        let specialized = IrType::Alias {
                            name: Symbol::new(format!("{}{}", name, self.ctx.generics(&args.args))),
                            ty: specialized
                        };
                        let specialized = self.ctx.types.insert(specialized);
                        //self.generic_types.get_mut(&ty).unwrap().specs.insert(args, specialized); 

                        self
                            .generic_args
                            .pop();

                        Ok(specialized)
                    },
                    None => Err(Diagnostic::error()
                        .with_message(format!("No generic template found matching arguments {}", self.ctx.generics(&args.args)))
                        .with_labels(vec![
                            Label::primary(file, span)
                        ])
                    )
                }
            },
            None => Ok(ty),
        }
    }

    /// Retrieve an existing type specialization or create a new one for the given generic type
    pub(super) fn specialize_fn(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        span: Span,
        fun: FunId,
        args: &UnresolvedGenericArgs,
        body: Option<&[Stmt]>
    ) -> Result<FunId, Diagnostic<FileId>> {
        let args = GenericArgs { args: args
            .args
            .iter()
            .map(|arg| self.resolve_type(arg, module, file, span))
            .collect::<Result<Vec<_>, _>>()?
        };
        match self.generic_funs.get(&fun) {
            Some(specs) => match specs.specs.get(&args) {
                Some(spec) => Ok(*spec),
                None => match specs.get(&args) {
                    Some(template) => {
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
                        
                        self
                            .generic_args
                            .push(bindings);
                        
                        let template = template.template.clone();
                        drop(specs);
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
                    },
                    None => Err(Diagnostic::error()
                        .with_message(format!("No generic template found matching arguments {}", self.ctx.generics(&args.args)))
                        .with_labels(vec![
                            Label::primary(file, span)
                        ])
                    )
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

impl<T, V> GenericSpecializations<T, V> {
    /// Get a template for the given template arguments
    pub fn get(&self, args: &GenericArgs) -> Option<&GenericTemplate<T>> {
        for spec in self.templates.iter() {
            if spec.matches(args) {
                return Some(spec)
            }
        }

        None
    }
    
    /// Create a new empty group of generic specializations
    pub fn new(name: Symbol, params: Vec<Symbol>) -> Self {
        Self {
            name,
            params,
            templates: BTreeSet::new(),
            specs: HashMap::new(),
        }
    }
    
    /// Add a template specialization for the given bounds
    pub fn add_spec(&mut self, bounds: Vec<GenericBound>, template: T) {
        self
            .templates
            .insert(GenericTemplate { params: bounds, template });
    }
}

impl<T> GenericTemplate<T> {
    /// Check if this group of bounds allows the given arguments
    pub fn matches(&self, args: &GenericArgs) -> bool {
        args.args.len() == self.params.len() &&
        self
            .params
            .iter()
            .zip(args.args.iter())
            .all(|(p, a)| p.matches(*a))
    }
}

impl GenericBound {
    /// Check if the given type is accepted by this bound
    pub fn matches(&self, ty: TypeId) -> bool {
        match self {
            Self::Is(other) => *other == ty,
            Self::Any => true,
        }
    }
}

impl GenericBound {
    pub(super) const fn ord(&self) -> usize {
        match self {
            Self::Is(..) => 1,
            Self::Any => 0,
        }
    }
}

impl<T> GenericTemplate<T> {
    pub(super) fn ord(&self) -> usize {
        self
            .params
            .iter()
            .map(GenericBound::ord)
            .sum()
    }
}

impl PartialOrd for GenericBound {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.ord().partial_cmp(&other.ord()) 
    } 
}

impl Ord for GenericBound {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.ord().cmp(&other.ord())
    }
}

impl<T> PartialEq for GenericTemplate<T> {
    fn eq(&self, other: &Self) -> bool {
        self
            .ord()
            .eq(&other.ord())
    }
}
impl<T> Eq for GenericTemplate<T> {}

impl<T> PartialOrd for GenericTemplate<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self
            .ord()
            .partial_cmp(&other.ord())
    }
}
impl<T> Ord for GenericTemplate<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.ord().cmp(&other.ord())
    }
}
