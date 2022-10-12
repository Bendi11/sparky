use crate::util::loc::Span;

use super::{IrFunRef, types::IrTypeRef};


/// Generic arguments passed to a definition template
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericArgs(Vec<IrTypeRef>);

/// Generic bounds associated with a specialization of a generic definition
#[derive(Clone, Debug)]
pub struct GenericBounds(Vec<GenericBound>);

/// A generic bound containing a reference to the position in the source file of the bound and the
/// bound itself
#[derive(Clone, Debug)]
pub struct GenericBound {
    pub span: Span,
    pub kind: GenericBoundKind,
}

/// A generic bound given as a parameter for a generic specialization
#[derive(Clone, Debug)]
pub enum GenericBoundKind {
    Any,
    Is(IrTypeRef),
    ValidType(IrTypeRef),
    ValidFun(IrFunRef),
}

/// A single generic specialization containing the generic bounds of the specialization and the
/// template body
#[derive(Clone, Debug)]
pub struct GenericSpec<Template> {
    pub bounds: GenericBounds,
    pub template: Template,
}

/// Container with definition's data and all specializations
#[derive(Clone, Debug)]
pub struct Generics<Def, Template> {
    pub def: Def,
    pub specializations: Vec<GenericSpec<Template>>, 
}
