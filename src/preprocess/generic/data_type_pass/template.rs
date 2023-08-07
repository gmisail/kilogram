use crate::ast::untyped::{ast_type::AstType, untyped_node::UntypedNode};
use crate::preprocess::generic::template::{substitute_all, Template};

#[derive(Debug)]
pub struct RecordTemplate {
    type_params: Vec<String>,
    fields: Vec<(String, AstType)>,
}

impl RecordTemplate {
    pub fn new(type_params: Vec<String>, fields: Vec<(String, AstType)>) -> RecordTemplate {
        RecordTemplate {
            type_params,
            fields,
        }
    }
}

impl Template for RecordTemplate {
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

                UntypedNode::RecordDeclaration(
                    head.to_string(),
                    self.fields
                        .iter()
                        .map(|(field_name, field_type)| {
                            let original_type =
                                substitute_all(field_type.clone(), &substitution_pairs);

                            (
                                field_name.clone(),
                                original_type.convert_generic_to_concrete(),
                            )
                        })
                        .collect(),
                    self.type_params.clone(),
                    Box::new(self.substitute(tail, body)),
                )
            }

            [] => body,
        }
    }
}
