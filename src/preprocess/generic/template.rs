use std::collections::HashMap;

use crate::ast::untyped::{ast_type::AstType, untyped_node::UntypedNode};

pub struct RecordTemplate {
    name: String,
    type_params: Vec<String>,
    fields: Vec<(String, AstType)>,
}

impl RecordTemplate {
    pub fn new(
        name: String,
        type_params: Vec<String>,
        fields: Vec<(String, AstType)>,
    ) -> RecordTemplate {
        RecordTemplate {
            name,
            type_params,
            fields,
        }
    }

    pub fn substitute(
        &self,
        type_param_values: &Vec<AstType>,
        body: UntypedNode,
    ) -> Result<UntypedNode, String> {
        if self.type_params.len() != type_param_values.len() {
            return Err("Mismatched type parameters.".into());
        }

        let substitutions: HashMap<String, AstType> = self
            .type_params
            .iter()
            .cloned()
            .zip(type_param_values.clone())
            .collect();

        let concrete_fields = self
            .fields
            .iter()
            .map(|field @ (field_name, field_type)| {
                if let AstType::Base(type_param) = field_type {
                    if let Some(substitution) = substitutions.get(type_param) {
                        return (field_name.clone(), substitution.clone());
                    }
                }

                field.clone()
            })
            .collect();

        Ok(UntypedNode::RecordDeclaration(
            "TEMP".into(),
            concrete_fields,
            self.type_params.clone(),
            Box::new(body),
        ))
    }
}

/// Creates a concrete type from type parameters: Map(String, Int) => Map_String_Int
fn create_concrete_type_name(base_type: String, sub_types: &[String]) -> String {
    format!("{base_type}{}", sub_types.join("_"))
}
