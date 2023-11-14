use crate::ast::untyped::ast_type::AstType;
use crate::ast::untyped::untyped_node::UntypedNode;
use crate::preprocess::generic::template::{substitute_all, Template};
use std::collections::HashSet;

#[derive(Debug)]
pub struct EnumTemplate {
    type_params: Vec<String>,
    variants: Vec<(String, Vec<AstType>)>,
}

impl EnumTemplate {
    pub fn new(type_params: Vec<String>, variants: Vec<(String, Vec<AstType>)>) -> EnumTemplate {
        EnumTemplate {
            type_params,
            variants,
        }
    }

    /// When registering a generic, check if it contains generic fields. This indicates a nested
    /// type that contains a generic, i.e. `List['T]` containing `Cons('T, List['T])`
    pub fn is_instance_generic(&self, ast_type: &AstType) -> bool {
        let param_set: HashSet<String> = HashSet::from_iter(self.type_params.iter().cloned());
        ast_type.is_generic(&param_set)
    }
}

impl Template for EnumTemplate {
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

                UntypedNode::EnumDeclaration(
                    head.to_string(),
                    self.variants
                        .iter()
                        .map(|(variant_name, variant_types)| {
                            let original_types: Vec<AstType> = variant_types
                                .iter()
                                .map(|variant_type| {
                                    substitute_all(variant_type.clone(), &substitution_pairs)
                                        .as_concrete()
                                })
                                .collect();

                            (variant_name.clone(), original_types)
                        })
                        .collect(),
                    Vec::new(),
                    Box::new(self.substitute(tail, body)),
                )
            }

            [] => body,
        }
    }
}
