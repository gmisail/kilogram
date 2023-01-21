use std::collections::HashMap;

use super::expr_type::Type;
use crate::ast;

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

    pub fn get_variable(&self, name: &String) -> Result<&Type, String> {
        match self.variables.get(name) {
            Some(var_type) => Ok(var_type),
            None => Err(format!("Can't find variable with name '{}'", name)),
        }
    }

    pub fn get_record(&self, name: &String) -> Result<&Type, String> {
        match self.records.get(name) {
            Some(record_type) => Ok(record_type),
            None => Err(format!("Can't find record with name '{}'", name)),
        }
    }

    // Creates an internal Record type based off of its AST representation.
    fn add_record(&mut self, name: &String, fields: &[(String, ast::Type)]) -> Result<(), String> {
        let record_types = fields
            .iter()
            .map(|(name, type_decl)| {
                (
                    name.clone(),
                    Box::new(self.from_ast_type(type_decl).unwrap().clone()),
                )
            })
            .collect();

        match self
            .records
            .insert(name.clone(), Type::Record(name.clone(), record_types))
        {
            Some(_) => Err(format!("Record '{}' already defined.", name)),
            None => Ok(()),
        }
    }

    // Add a variable to the type-checking context.
    fn add_variable(&mut self, var_name: String, var_type: Type) -> Result<(), String> {
        match self.variables.insert(var_name.clone(), var_type) {
            Some(_) => Err(format!("Variable '{}' already defined.", var_name)),
            None => Ok(()),
        }
    }

    // Converts an AST type (int, string, ...) into a actual type.
    fn from_ast_type(&self, t: &ast::Type) -> Result<&Type, String> {
        match t {
            ast::Type::Base(name) => match name.as_str() {
                "int" => Ok(&Type::Integer),
                "float" => Ok(&Type::Float),
                "string" => Ok(&Type::Str),
                "bool" => Ok(&Type::Boolean),
                _ => self.get_record(name),
            },
            _ => Err("Unable to convert type to internal type.".to_string()),
        }
    }

    pub fn resolve_type(&mut self, expression: ast::Expression) -> Result<&Type, String> {
        match expression {
            ast::Expression::Integer(_) => Ok(&Type::Integer),
            ast::Expression::Float(_) => Ok(&Type::Float),
            ast::Expression::Str(_) => Ok(&Type::Str),
            ast::Expression::Boolean(_) => Ok(&Type::Boolean),
            ast::Expression::Group(inner) => self.resolve_type(*inner),
            ast::Expression::Variable(name) => self.get_variable(&name),
            ast::Expression::Let(var_name, var_ast_type, var_value, body) => {
                let var_type = self.from_ast_type(&var_ast_type)?.clone();
                let value_type = self.resolve_type(*var_value)?;

                if var_type == *value_type {
                    self.add_variable(var_name, var_type)?;
                    self.resolve_type(*body)
                } else {
                    Err(format!("Can't define variables with incompatible types! Variable is defined as type <insert type here>, but you're assigning it to a value of type <insert different type>."))
                }
            }
            ast::Expression::RecordDeclaration(name, fields, body) => {
                self.add_record(&name, &fields)?;
                self.resolve_type(*body)
            }
            _ => Err("Unable to resolve type for expression.".to_string()),
        }
    }
}
