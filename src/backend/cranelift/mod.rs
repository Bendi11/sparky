use cranelift::{
    frontend::{FunctionBuilder, FunctionBuilderContext}, prelude::{Value, InstBuilder},
};

use cranelift_object::{ObjectModule, ObjectBuilder};

use crate::{ir::{IRContext, Node, lower::SemanticResult}, error::DiagnosticManager, ast::NumberLiteral};

/// Structure that generates native machine code from a lowered
/// AST using [cranelift]
pub struct CraneliftBackend<'files> {
    /// The cranelift module currently being built
    module: ObjectModule,
    /// The lowered AST context containing all parsed data
    ctx: IRContext,
    /// Diagnostics handler
    diagnostic: DiagnosticManager<'files>,
}

impl<'files> CraneliftBackend<'files> {
    
    /// Compile one AST node
    fn compile_node(&mut self, node: &Node, builder: &mut FunctionBuilder<'_>) -> SemanticResult<Option<Value>> {
        Ok(match node {
            Node::NumberLiteral(NumberLiteral::Integer(bigint)) => {
                Some(builder.ins.iconst(bigint., N))
            },
            Node::NumberLiteral(NumberLiteral::Float(f)) => {
                Some(builder.ins().f64const(*f))
            },
            _ => unimplemented!("{:?} not implemented yet", node),
        })
    }
}
