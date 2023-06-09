use crate::ast::untyped::ast_type::AstType;
use crate::ast::untyped::untyped_node::UntypedNode;

use crate::preprocess::generic::template::{substitute_all, Template};

#[derive(Debug)]
pub struct FunctionTemplate {
    type_params: Vec<String>,
    func_params: Vec<(String, AstType)>,
    func_return: AstType,
    func_body: UntypedNode,
}

impl FunctionTemplate {
    pub fn new(
        type_params: Vec<String>,
        func_params: Vec<(String, AstType)>,
        func_return: AstType,
        func_body: UntypedNode,
    ) -> FunctionTemplate {
        FunctionTemplate {
            type_params,
            func_params,
            func_return,
            func_body,
        }
    }
}

impl FunctionTemplate {
    // From a list of generic parameters and substitutions, get a list of concrete types.
    fn resolve_parameters(&self, substitutions: &Vec<(String, AstType)>) -> Vec<(String, AstType)> {
        self.func_params
            .iter()
            .map(|(param_name, param_type)| {
                let original_type = substitute_all(param_type.clone(), &substitutions);

                (
                    param_name.clone(),
                    original_type.convert_generic_to_concrete(),
                )
            })
            .collect()
    }

    fn resolve_return_type(&self, substitutions: &Vec<(String, AstType)>) -> AstType {
        substitute_all(self.func_return.clone(), &substitutions)
    }

    fn substitute_body(&self) -> UntypedNode {
        UntypedNode::Integer(0)
    }
}

impl Template for FunctionTemplate {
    fn substitute(&self, variants: &[AstType], body: UntypedNode) -> UntypedNode {
        match variants {
            [head, tail @ ..] => {
                let types = if let AstType::Generic(_, sub_types) = &head {
                    sub_types
                } else {
                    panic!("Expected type be generic.")
                };

                let substitution_pairs = self
                    .type_params
                    .iter()
                    .cloned()
                    .zip(types.iter().cloned())
                    .collect::<Vec<(String, AstType)>>();

                UntypedNode::FunctionDeclaration(
                    head.to_string(),
                    Vec::new(),
                    self.resolve_return_type(&substitution_pairs),
                    self.resolve_parameters(&substitution_pairs),
                    Box::new(self.substitute_body()),
                    Box::new(self.substitute(tail, body)),
                )
            }

            [] => body,
        }
    }
}
