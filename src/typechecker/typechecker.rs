use crate::ast::{self, Expression};

use std::{collections::HashMap, ops::Deref};

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
    fn add_record(&mut self, name: &String, fields: &Vec<(String, ast::Type)>) -> bool {
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

    fn add_variable(&mut self, var_name: &String, var_type: Type) -> bool {
        if self.variables.contains_key(var_name) {
            false
        } else {
            println!("Variable {} added.", var_name);

            self.variables.insert(var_name.clone(), var_type);

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

    pub fn resolve_type(&mut self, expression: ast::Expression) -> Result<&Type, String> {
        match expression {
            ast::Expression::Integer(_) => Ok(&Type::Integer),
            ast::Expression::Float(_) => Ok(&Type::Float),
            ast::Expression::Str(_) => Ok(&Type::Str),
            ast::Expression::Boolean(_) => Ok(&Type::Boolean),
            ast::Expression::Group(inner) => self.resolve_type(*inner),
            ast::Expression::Variable(name) => {
                match self.variables.get(&name) {
                    Some(var_type) => Ok(var_type),
                    None => Err(format!("Variable '{}' is not in scope.", name))
                }
            },
            ast::Expression::Let(var_name, var_type, _, body) => {
                // TODO: validate that var_type == type(var_value) 
                
                if !self.add_variable(&var_name, self.from_ast_type(&var_type).unwrap().clone()) {
                    Err(format!("Record '{}' already defined.", var_name))
                } else {
                    self.resolve_type(*body)
                }
            },
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
