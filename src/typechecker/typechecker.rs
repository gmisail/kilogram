use crate::ast;

use std::collections::HashMap;

use super::expr_type::Type;

pub struct Typechecker {
    variables: HashMap<String, Type>,
    records: HashMap<String, Type>,
}

impl Typechecker {
    pub fn new() -> Self {
        Typechecker {
            variables: HashMap::new(),
            records: HashMap::new(),
        }
    }

    pub fn get_variable(&self, name: &String) -> Option<&Type> {
        self.variables.get(name)
    }

    pub fn get_record(&self, name: &String) -> Option<&Type> {
        self.records.get(name)
    }

    // Creates an internal Record type based off of its AST representation.
    pub fn add_record(&mut self, name: &String, fields: &Vec<(String, ast::Type)>) -> bool {
        if self.records.contains_key(name) {
            false
        } else {
            println!("Record {} added.", name);

            let record_types = fields
                .iter()
                .map(|(name, type_decl)| {
                    (
                        name.clone(),
                        Box::new(self.from_ast_type(type_decl).unwrap().clone()),
                    )
                })
                .collect();

            self.records
                .insert(name.clone(), Type::Record(name.clone(), record_types));

            true
        }
    }

    // Converts an AST type (int, string, ...) into a actual type.
    fn from_ast_type(&self, t: &ast::Type) -> Option<&Type> {
        match t {
            ast::Type::Base(name) => match name.as_str() {
                "int" => Some(&Type::Integer),
                "float" => Some(&Type::Float),
                "string" => Some(&Type::Str),
                "bool" => Some(&Type::Boolean),
                _ => self.records.get(name),
            },
            _ => panic!("Unhandled"),
        }
    }

    pub fn resolve_type(&mut self, expression: ast::Expression) -> Result<Type, String> {
        match expression {
            ast::Expression::Integer(_) => Ok(Type::Integer),
            ast::Expression::Float(_) => Ok(Type::Float),
            ast::Expression::Str(_) => Ok(Type::Str),
            ast::Expression::Boolean(_) => Ok(Type::Boolean),
            ast::Expression::Group(inner) => self.resolve_type(*inner),
            ast::Expression::RecordDeclaration(name, fields, body) => {
                if !self.add_record(&name, &fields) {
                    Err(format!("Record '{}' already defined.", name))
                } else {
                    self.resolve_type(*body)
                }
            }
            _ => Err("Unable to resolve type for expression.".to_string()),
        }
    }
}
