use std::{collections::BTreeSet, fmt::Display};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use hashbrown::HashMap;

use crate::{ir::{TypeId, types::IrType, FunId, IrFun, IrContext, GlobalId, IrGlobal, IrStmt, IrStmtKind, value::{IrExpr, IrExprKind}}, Symbol, util::{files::FileId, loc::Span}, ast::{Stmt, UnresolvedGenericArgs, Expr}};

use super::{IrLowerer, IntermediateModuleId, IntermediateDefId};

/// Structure containing a list of generic arguments passed to a templated definition
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericArgs {
    args: Vec<TypeId>,
}

/// A bound that can be placed on a generic type parameter
#[derive(Clone,)]
pub enum GenericBound {
    /// Test if the given expression is valid
    Can(IntermediateDefId, GenericArgs),
    /// Type must be equal to the given type
    Is(TypeId),
    /// There is no bound on the given type
    Any,
}

/// A collection of [IntermediateGenericBound]s that limit the parameters of a generic value
#[derive(Clone)]
pub struct GenericTemplate<T> {
    /// The bounds for this template to be applied
    pub(super) params: Vec<GenericBound>,
    template: T,
}

/// Structure containing multiple specializations of a generic templated value
#[derive(Clone)]
pub struct GenericSpecializations<Template, Value> {
    /// Name of this template
    pub name: Symbol,
    /// Parameter names and length
    pub params: Vec<Symbol>,
    /// Existing templates for this value
    pub(super) templates: Vec<GenericTemplate<Template>>,
    /// Existing specializations for this generic
    pub(super) specs: HashMap<GenericArgs, Value>,
}


impl<'ctx> IrLowerer<'ctx> {
    pub fn resolve_args(&mut self, module: IntermediateModuleId, file: FileId, span: Span, args: &UnresolvedGenericArgs) -> Result<GenericArgs, Diagnostic<FileId>> {
        Ok(GenericArgs {
            args: args
                .args
                .iter()
                .map(|arg| self.resolve_type(arg, module, file, span))
                .collect::<Result<Vec<_>, _>>()?
        })
    }
    /// Retrieve an existing type specialization or create a new one for the given generic type
    pub(super) fn specialize_type(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        span: Span,
        ty: TypeId,
        args: GenericArgs
    ) -> Result<TypeId, Diagnostic<FileId>> {
        let specs = self.generic_types.get(&ty).cloned();
        match specs {
            Some(specs) => match specs.specs.get(&args) {
                Some(spec) if *spec != IrContext::INVALID => Ok(*spec),
                _ => match self.get(module, file, &specs, &args) {
                    Some(template) => {
                        let template = template.clone();
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
                        self.generic_types.get_mut(&ty).unwrap().specs.insert(args, specialized); 

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

    /// Specialize a global value with the given type arguments
    pub(super) fn specialize_global(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        span: Span,
        glob: GlobalId,
        args: GenericArgs
    ) -> Result<GlobalId, Diagnostic<FileId>> {
        let specs = self.generic_globs.get(&glob).cloned();
        match specs {
            Some(specs) => match specs.specs.get(&args) {
                Some(spec) => Ok(*spec),
                _ => match self.get(module, file, &specs, &args) {
                    Some(template) => {
                        let template = template.clone();
                        if args.args.len() != specs.params.len() {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "Incorrect number of generic arguments passed to global variable template {}; expected {}, got {}",
                                    self.ctx[glob].name,
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
                        let old_bb = self.bb;
                        self.bb = Some(self.ctx[self.global_setup_fun].body.as_ref().unwrap().entry);
                        let specialized = self.lower_expr(module, file, self.global_setup_fun, &template)?;
                        let specialized_id = IrGlobal {
                            name: Symbol::new(format!("{}{}", name, self.ctx.generics(&args.args))),
                            ty: specialized.ty,
                        };
                        let specialized_id = self.ctx.globals.insert(specialized_id);

                        self.ctx[self.bb.unwrap()].stmts.push(IrStmt {
                            span: Span::from(0..0),
                            kind: IrStmtKind::Write {
                                ptr: IrExpr {
                                    span: Span::from(0..0),
                                    ty: specialized.ty,
                                    kind: IrExprKind::Global(specialized_id),
                                },
                                val: specialized.clone(),
                            }
                        });
                        self.generic_globs.get_mut(&glob).unwrap().specs.insert(args, specialized_id); 

                        self.bb = old_bb;
                        self
                            .generic_args
                            .pop();

                        Ok(specialized_id)
                    },
                    None => Err(Diagnostic::error()
                        .with_message(format!("No generic template found matching arguments {}", self.ctx.generics(&args.args)))
                        .with_labels(vec![
                            Label::primary(file, span)
                        ])
                    )
                }
            },
            None => Ok(glob),
        }
    }

    /// Retrieve an existing type specialization or create a new one for the given generic type
    pub(super) fn specialize_fn(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        span: Span,
        fun: FunId,
        args: GenericArgs,
        body: Option<&[Stmt]>
    ) -> Result<FunId, Diagnostic<FileId>> {
        let specs = self.generic_funs.get(&fun).cloned();
        match specs {
            Some(specs) => match specs.specs.get(&args) {
                Some(spec) => Ok(*spec),
                None => match self.get(module, file, &specs, &args) {
                    Some(template) => {
                        let template = template.clone();
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
        
    /// Create a new empty group of generic specializations
    pub fn new(name: Symbol, params: Vec<Symbol>) -> Self {
        Self {
            name,
            params,
            templates: Vec::new(),
            specs: HashMap::new(),
        }
    }
    
    /// Add a template specialization for the given bounds
    pub fn add_spec(&mut self, bounds: Vec<GenericBound>, template: T) {
        let template = GenericTemplate { params: bounds, template };
        self.templates.push(template); 
    }
}

impl<T> GenericTemplate<T> {
    
}

impl<'ctx> IrLowerer<'ctx> {
    

    /// Get a template for the given template arguments
    pub fn get<'a, T, V>(&'a mut self, module: IntermediateModuleId, file: FileId, specs: &'a GenericSpecializations<T, V>, args: &GenericArgs) -> Option<&'a GenericTemplate<T>> {
        let mut best_score = None;
        for spec in specs.templates.iter() {
            if self.matches(module, file, spec, args) {
                if best_score.map(|(score, _)| score < spec.ord()).unwrap_or(true) {
                    best_score = Some((spec.ord(), spec));
                }
            }
        }

        best_score.map(|(_, spec)| spec)
    }


    /// Check if the given type is accepted by this bound
    pub fn matches_bound(&mut self, module: IntermediateModuleId, file: FileId, bound: &GenericBound, ty: TypeId) -> bool {
        let span = Span::from(0..0);
        match bound {
            GenericBound::Can(id, args) => match id {
                IntermediateDefId::Type(ty) => self.specialize_type(module, file, span, *ty, args.clone()).is_ok(),
                IntermediateDefId::Fun(fun) => self.specialize_fn(module, file, span, *fun, args.clone(), None).is_ok(),
                IntermediateDefId::Global(glob) => self.specialize_global(module, file, span, *glob, args.clone()).is_ok(),
                _ => false,
            },
            GenericBound::Is(other) => *other == ty,
            GenericBound::Any => true,
        }
    }

    /// Check if this group of bounds allows the given arguments
    pub fn matches<T>(&mut self, module: IntermediateModuleId, file: FileId, template: &GenericTemplate<T>, args: &GenericArgs) -> bool {
        args.args.len() == template.params.len() &&
        template
            .params
            .iter()
            .zip(args.args.iter())
            .all(|(p, a)| self.matches_bound(module, file, p, *a))
    }
}

impl GenericBound {
    pub(super) const fn ord(&self) -> usize {
        match self {
            Self::Can(..) => 1,
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


impl<T> PartialEq for GenericTemplate<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ord().eq(&other.ord()) 
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
