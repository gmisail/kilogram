use std::collections::BTreeSet;

use crate::ast::untyped::{ast_type::AstType, untyped_node::UntypedNode};

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

    fn substitute_all(&self, subject_type: AstType, types: &[(String, AstType)]) -> AstType {
        match types {
            [(type_name, subtituted_type), tail @ ..] => {
                let result = subject_type.substitute_type_parameter(type_name, subtituted_type);

                self.substitute_all(result, tail)
            }

            [] => subject_type,
        }
    }

    pub fn substitute(&self, variants: &mut BTreeSet<AstType>, body: UntypedNode) -> UntypedNode {
        if variants.len() == 0 {
            body
        } else {
            let head = variants.pop_first().unwrap().clone();

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
                            self.substitute_all(field_type.clone(), &substitution_pairs);

                        (
                            field_name.clone(),
                            original_type.convert_generic_to_concrete(),
                        )
                    })
                    .collect(),
                self.type_params.clone(),
                Box::new(self.substitute(variants, body)),
            )
        }
    }
}
