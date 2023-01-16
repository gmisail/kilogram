use std::collections::HashMap;

use super::expr_type::Type;

pub struct Context {
    variables: HashMap<String, Type>,
    records: HashMap<String, String>,
}

impl Context {
    pub fn get_variable(&self, name: String) -> Option<&Type> {
        self.variables.get(&name)
    }
}
