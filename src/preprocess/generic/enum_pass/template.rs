use crate::ast::untyped::ast_type::AstType;
use crate::ast::untyped::untyped_node::UntypedNode;
use crate::preprocess::generic::template::{substitute_all, Template};

#[derive(Debug)]
pub struct EnumTemplate {
    type_params: Vec<String>,
    fields: Vec<(String, AstType)>,
}

impl EnumTemplate {
    pub fn new(type_params: Vec<String>, fields: Vec<(String, AstType)>) -> EnumTemplate {
        EnumTemplate {
            type_params,
            fields,
        }
    }
}

impl Template for EnumTemplate {
    fn substitute(&self, variants: &[AstType], body: UntypedNode) -> UntypedNode {
        todo!()
    }
}
