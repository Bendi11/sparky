use cranelift::{
    frontend::{FunctionBuilder, FunctionBuilderContext},
};

use cranelift_object::{ObjectModule, ObjectBuilder};

use crate::ir::IRContext;

/// Structure that generates native machine code from a lowered
/// AST using [cranelift]
pub struct CraneliftBackend {
    /// The cranelift module currently being built
    module: ObjectModule,
    /// The lowered AST context containing all parsed data
    ctx: IRContext,
}

impl CraneliftBackend {
     
}
