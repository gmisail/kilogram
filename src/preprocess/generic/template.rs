use std::collections::HashMap;

use crate::ast::untyped::ast_type::AstType;

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

    pub fn substitute(substitutions: HashMap<String, AstType>) {
        todo!()
    }
}
