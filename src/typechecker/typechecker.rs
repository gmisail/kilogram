use std::collections::HashMap;
use std::borrow::Borrow;
use std::rc::Rc;

use super::datatype::Type;
use super::rules::{check_binary, check_logical, check_unary};
use crate::ast;

pub struct Typechecker {
    primitives: HashMap<&'static str, Rc<Type>>,
    variables: HashMap<String, Rc<Type>>,
    records: HashMap<String, Rc<Type>>,
}

impl Typechecker {
    pub fn new() -> Self {
        let mut primitives = HashMap::new();
        primitives.insert("int", Rc::new(Type::Integer));
        primitives.insert("float", Rc::new(Type::Float));
        primitives.insert("bool", Rc::new(Type::Boolean));
        primitives.insert("string", Rc::new(Type::Str));

        Typechecker {
            primitives,
            variables: HashMap::new(),
            records: HashMap::new(),
        }
    }

    pub fn get_variable(&self, name: &String) -> Result<Rc<Type>, String> {
        match self.variables.get(name) {
            Some(var_type) => Ok(var_type.clone()),
            None => Err(format!("Can't find variable with name '{}'", name)),
        }
    }

    pub fn get_record(&self, name: &String) -> Result<Rc<Type>, String> {
        match self.records.get(name) {
            Some(record_type) => Ok(record_type.clone()),
            None => Err(format!("Can't find record with name '{}'", name)),
        }
    }

    // Creates an internal Record type based off of its AST representation.
    fn add_record(&mut self, name: &String, fields: &[(String, ast::Type)]) -> Result<(), String> {
        let record_types = fields
            .iter()
            .map(|(name, type_decl)| (name.clone(), self.from_ast_type(type_decl).unwrap().clone()))
            .collect();

        match self.records.insert(
            name.clone(),
            Rc::new(Type::Record(name.clone(), record_types)),
        ) {
            Some(_) => Err(format!("Record '{}' already defined.", name)),
            None => Ok(()),
        }
    }

    // Add a variable to the type-checking context.
    fn add_variable(&mut self, var_name: &String, var_type: Rc<Type>) -> Result<(), String> {
        match self.variables.insert(var_name.clone(), var_type) {
            Some(_) => Err(format!("Variable '{}' already defined.", var_name)),
            None => Ok(()),
        }
    }

    fn remove_variable(&mut self, var_name: String) -> Result<(), String> {
        match self.variables.remove(&var_name) {
            None => Err(format!("Can't remove variable '{}' since it is not defined.", var_name)),
            Some(_) => Ok(())
        }
    }

    // Converts an AST type (int, string, ...) into a actual type.
    fn from_ast_type(&self, t: &ast::Type) -> Result<Rc<Type>, String> {
        match t {
            ast::Type::Base(name) => match name.as_str() {
                // TODO: make this into one rule for primitives
                "int" => Ok(self.primitives.get("int").unwrap().clone()),
                "float" => Ok(self.primitives.get("float").unwrap().clone()),
                "string" => Ok(self.primitives.get("string").unwrap().clone()),
                "bool" => Ok(self.primitives.get("bool").unwrap().clone()),

                _ => self.get_record(name),
            },

            ast::Type::Function(ast_argument_types, ast_return_type) => {
                let mut argument_types = vec![];

                for ast_argument in ast_argument_types {
                    argument_types.push(self.from_ast_type(&ast_argument)?);
                }

                let return_type = self.from_ast_type(&ast_return_type)?;

                Ok(Rc::new(Type::Function(argument_types, return_type)))
            }
            _ => Err("Unable to convert type to internal type.".to_string()),
        }
    }

    pub fn resolve_type(&mut self, expression: ast::Expression) -> Result<Rc<Type>, String> {
        match expression {
            ast::Expression::Integer(_) => Ok(self.primitives.get("int").unwrap().clone()),
            ast::Expression::Float(_) => Ok(self.primitives.get("float").unwrap().clone()),
            ast::Expression::Str(_) => Ok(self.primitives.get("string").unwrap().clone()),
            ast::Expression::Boolean(_) => Ok(self.primitives.get("bool").unwrap().clone()),
            ast::Expression::Group(inner) => self.resolve_type(*inner),
            ast::Expression::Variable(name) => self.get_variable(&name),

            ast::Expression::Unary(expr, operator) => {
                let expr_type = self.resolve_type(*expr)?;

                if check_unary(&operator, expr_type.clone()) {
                    Ok(expr_type)
                } else {
                    Err(format!(
                        "Cannot apply unary operator {} to expression of type {}.",
                        operator, *expr_type
                    ))
                }
            }
            ast::Expression::Binary(left_expr, operator, right_expr) => {
                let left_type = self.resolve_type(*left_expr)?;
                let right_type = self.resolve_type(*right_expr)?;

                if check_binary(&operator, left_type.clone(), right_type) {
                    // Type of binary operation depends on the operator.
                    match operator {
                        ast::BinaryOperator::Add
                        | ast::BinaryOperator::Sub
                        | ast::BinaryOperator::Mult
                        | ast::BinaryOperator::Div => Ok(left_type),

                        ast::BinaryOperator::Greater
                        | ast::BinaryOperator::GreaterEq
                        | ast::BinaryOperator::Less
                        | ast::BinaryOperator::LessEq
                        | ast::BinaryOperator::Equality
                        | ast::BinaryOperator::NotEqual => {
                            Ok(self.primitives.get("bool").unwrap().clone())
                        }
                    }
                } else {
                    Err("Binary operator not compatible with types.".to_string())
                }
            }
            ast::Expression::Logical(left_expr, operator, right_expr) => {
                let left_type = self.resolve_type(*left_expr)?;
                let right_type = self.resolve_type(*right_expr)?;

                if check_logical(&operator, left_type, right_type) {
                    Ok(self.primitives.get("bool").unwrap().clone())
                } else {
                    Err("Logical operation must be between two boolean conditions.".to_string())
                }
            }

            ast::Expression::If(condition, then_expr, else_expr) => {
                let condition_type = self.resolve_type(*condition)?;

                if *condition_type == Type::Boolean {
                    let then_type = self.resolve_type(*then_expr)?;
                    let else_type = self.resolve_type(*else_expr)?;

                    if *then_type == *else_type {
                        Ok(then_type)
                    } else {
                        Err(format!("Branches in 'if' statement must have the same type! Got branches with type {} and {}.", *then_type, *else_type))
                    }
                } else {
                    Err(format!("Expected condition in 'if' statement to be of type boolean, but was {} instead.", *condition_type))
                }
            }

            ast::Expression::Let(var_name, var_ast_type, var_value, body) => {
                let var_type = self.from_ast_type(&var_ast_type)?.clone();
                let value_type = self.resolve_type(*var_value)?;

                if *var_type == *value_type {
                    self.add_variable(&var_name, var_type)?;
                    self.resolve_type(*body)
                } else {
                    Err(format!("Can't define variables with incompatible types! Variable is defined as type {}, but you're assigning it to a value of type {}.", *var_type, *value_type))
                }
            }

            ast::Expression::Function(_, ast_return_type, ast_argument_types, body) => {
                let mut argument_types = vec![];

                // Resolve types & add parameters to scope.
                for (arg_name, arg_ast_type) in &ast_argument_types {
                    let arg_type = self.from_ast_type(&arg_ast_type)?;

                    argument_types.push(arg_type.clone());

                    self.add_variable(arg_name, arg_type)?;
                }

                let body_type = self.resolve_type(*body)?;
                let return_type = self.from_ast_type(&ast_return_type)?;

                // Since the expression has been evaluated, pop parameters from scope.
                for (arg_name, _) in ast_argument_types {
                    self.remove_variable(arg_name)?;
                }

                if *body_type == *return_type {
                    Ok(Rc::new(Type::Function(argument_types, return_type)))
                } else {
                    Err("Function return type and actual type returned do not match.".to_string())
                }
            }
            ast::Expression::FunctionCall(parent, parameters) => {
                // Verify that the parameter & argument types match.
                match self.resolve_type(*parent)?.borrow() {
                    Type::Function(arguments, return_type) => {
                        // Check that the types of the expressions match the expected type.
                        for (target_type, parameter) in arguments.iter().zip(parameters) {
                            let resolved_type = self.resolve_type(*parameter.clone())?;

                            if *target_type != resolved_type {
                                return Err(format!(
                                    "Invalid parameter type, expected {} but got {}.",
                                    target_type, resolved_type
                                ));
                            }
                        }

                        Ok(return_type.clone())
                    }

                    _ => Err("Cannot call non-function.".to_string()),
                }
            }

            ast::Expression::RecordInstance(name, fields) => {
                let record_type = self.get_record(&name)?;
                let mut field_types = HashMap::new();

                for (field_name, field_ast_type) in fields {
                    field_types.insert(field_name, self.resolve_type(*field_ast_type)?);
                }

                if Type::Record(name, field_types) == *record_type {
                    Ok(record_type)
                } else {
                    Err("Records don't match.".to_string())
                }
            }
            ast::Expression::RecordDeclaration(name, fields, body) => {
                self.add_record(&name, &fields)?;
                self.resolve_type(*body)
            }

            ast::Expression::Get(field, parent) => match self.resolve_type(*parent)?.borrow() {
                Type::Record(name, fields) => match fields.get(&field) {
                    Some(field_type) => Ok(field_type.clone()),
                    None => Err(format!(
                        "Can't find field {} in record of type {}",
                        field, name
                    )),
                },

                invalid_type => Err(format!("Can't access fields from type {}.", invalid_type)),
            },
        }
    }
}
