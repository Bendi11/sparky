use super::*;

impl<'c> Compiler<'c> {
    /// Get a struct type from the given path
    pub fn get_struct(&self, name: impl AsRef<str>) -> Option<(StructType<'c>, Container)> {
        let name = name.as_ref();
        self.struct_types.get(name).cloned()
    }

    /// Get a union type from the given path
    pub fn get_union(&self, name: impl AsRef<str>) -> Option<(StructType<'c>, Container)> {
        let name = name.as_ref();
        self.union_types.get(name).cloned()
    }

    /// Get a typedef'd type from the given path
    pub fn get_typedef(&self, name: impl AsRef<str>) -> Option<Type> {
        let name = name.as_ref();
        self.typedefs.get(name).cloned()
    }

    /// Get a struct type from the given path
    pub fn get_fun(&self, name: impl AsRef<str>) -> Option<(FunctionValue<'c>, FunProto)> {
        let name = name.as_ref();
        self.funs.get(name).cloned()
    }

    /// Get all type definitions and track them as opaque struct types
    pub fn get_opaques(&self, ast: Vec<Ast>) -> Vec<Ast> {
        ast.iter().for_each(|node| {
            match node {
                Ast::StructDec(container) => {
                    self.ctx.opaque_struct_type(&container.name);
                },
                Ast::UnionDec(container) => {
                    self.ctx.opaque_struct_type(&container.name);
                },
                _ => ()
            }
        });

        ast
    }

    pub fn get_type_bodies(&self, ast: Vec<Ast>) -> Vec<Ast> {
        ast.into_iter().filter(|node| match node {
            Ast::StructDec(container) => {

            },
        }).collect()
    }
}