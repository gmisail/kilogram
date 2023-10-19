use crate::ast::untyped::ast_type::AstType;
use crate::preprocess::generic::template::Template;
use std::collections::{BTreeSet, HashMap};

pub struct PassState<T: Template> {
    all_types: BTreeSet<AstType>,
    types: HashMap<String, Vec<AstType>>,
    templates: HashMap<String, T>,
}

impl<T: Template> PassState<T> {
    pub fn new() -> Self {
        Self {
            all_types: BTreeSet::new(),
            types: HashMap::new(),
            templates: HashMap::new(),
        }
    }

    pub fn register_type(&mut self, name: String, ast_type: AstType) {
        if !self.all_types.contains(&ast_type) {
            self.all_types.insert(ast_type.clone());

            self.types.entry(name).or_insert(Vec::new()).push(ast_type);
        }
    }

    pub fn get_types(&self, name: &String) -> Option<&Vec<AstType>> {
        self.types.get(name)
    }

    pub fn register_template(&mut self, name: String, template: T) -> Option<T> {
        self.templates.insert(name, template)
    }

    pub fn get_template(&self, name: &String) -> Option<&T> {
        self.templates.get(name)
    }
}
