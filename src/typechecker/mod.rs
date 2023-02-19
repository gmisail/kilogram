use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

mod error;
mod rules;

use crate::ast::operator::BinaryOperator;
use crate::ast::untyped_node::UntypedNode;
use crate::typed::data_type::DataType;
use crate::{ast::ast_type::AstType, typed::typed_node::TypedNode};
use rules::{check_binary, check_logical, check_unary};

pub struct Typechecker {
    primitives: HashMap<&'static str, Rc<DataType>>,
    stack: HashMap<String, Rc<DataType>>,
    pub records: HashMap<String, Rc<DataType>>,
    anonymous_records: Vec<String>,
    fresh_counter: i32,
}

impl Typechecker {
    pub fn new() -> Self {
        let mut primitives = HashMap::new();
        primitives.insert("int", Rc::new(DataType::Integer));
        primitives.insert("float", Rc::new(DataType::Float));
        primitives.insert("bool", Rc::new(DataType::Boolean));
        primitives.insert("string", Rc::new(DataType::Str));

        Typechecker {
            primitives,
            stack: HashMap::new(),
            records: HashMap::new(),
            anonymous_records: Vec::new(),
            fresh_counter: 0,
        }
    }

    pub fn get_variable(&self, name: &String) -> Result<Rc<DataType>, String> {
        match self.stack.get(name) {
            Some(var_type) => Ok(var_type.clone()),
            None => Err(format!("Can't find variable with name '{name}'")),
        }
    }

    pub fn get_record(&self, name: &String) -> Result<Rc<DataType>, String> {
        match self.records.get(name) {
            Some(record_type) => Ok(record_type.clone()),
            None => Err(format!("Can't find record with name '{name}'")),
        }
    }

    // Creates an internal Record type based off of its AST representation.
    fn add_record(&mut self, name: &String, fields: &[(String, AstType)]) -> Result<(), String> {
        let record_types = fields
            .iter()
            .map(|(name, type_decl)| (name.clone(), self.convert_ast_type(type_decl).unwrap()))
            .collect();

        match self.records.insert(
            name.clone(),
            Rc::new(DataType::Record(name.clone(), record_types)),
        ) {
            Some(_) => Err(format!("Record '{name}' already defined.")),
            None => Ok(()),
        }
    }

    // Creates a record with a unique name.
    fn add_anonymous_record(
        &mut self,
        field_types: BTreeMap<String, Rc<DataType>>,
    ) -> Result<String, String> {
        let fresh_name = format!("anonymous_{}", self.fresh_counter);
        self.fresh_counter += 1;

        let new_record = Rc::new(DataType::Record(fresh_name.clone(), field_types));

        for record in &self.anonymous_records {
            let anonymous_record = self.records.get(record).unwrap();

            // Duplicate record? Return the matching previously defined record.
            if *new_record == **anonymous_record {
                self.fresh_counter -= 1;

                return Ok(record.clone());
            }
        }

        self.anonymous_records.push(fresh_name.clone());

        self.records.insert(fresh_name.clone(), new_record);

        Ok(fresh_name)
    }

    // Add a variable to the type-checking context.
    fn add_variable(&mut self, var_name: &String, var_type: Rc<DataType>) -> Result<(), String> {
        match self.stack.insert(var_name.clone(), var_type) {
            Some(_) => Err(format!("Variable '{var_name}' already defined.")),
            None => Ok(()),
        }
    }

    fn remove_variable(&mut self, var_name: String) -> Result<(), String> {
        match self.stack.remove(&var_name) {
            None => Err(format!(
                "Can't remove variable '{var_name}' since it is not defined."
            )),
            Some(_) => Ok(()),
        }
    }

    // Converts an AST type (int, string, ...) into a actual type.
    fn convert_ast_type(&mut self, t: &AstType) -> Result<Rc<DataType>, String> {
        match t {
            AstType::Base(name) => match name.as_str() {
                // TODO: make this into one rule for primitives
                "int" => Ok(self.primitives.get("int").unwrap().clone()),
                "float" => Ok(self.primitives.get("float").unwrap().clone()),
                "string" => Ok(self.primitives.get("string").unwrap().clone()),
                "bool" => Ok(self.primitives.get("bool").unwrap().clone()),

                _ => self.get_record(name),
            },

            AstType::Function(ast_argument_types, ast_return_type) => {
                let mut argument_types = vec![];

                for ast_argument in ast_argument_types {
                    argument_types.push(self.convert_ast_type(ast_argument)?);
                }

                let return_type = self.convert_ast_type(ast_return_type)?;

                Ok(Rc::new(DataType::Function(argument_types, return_type)))
            }

            // Anonymous type, register it with a temporary name.
            AstType::Record(fields) => {
                let mut field_types = BTreeMap::new();

                for (field_name, field_type) in fields {
                    field_types.insert(field_name.clone(), self.convert_ast_type(field_type)?);
                }

                let fresh_name = self.add_anonymous_record(field_types)?;

                self.get_record(&fresh_name)
            }

            _ => Err("Unable to convert type to internal type.".to_string()),
        }
    }

    fn resolve_primitive(&self, type_name: &str) -> Rc<DataType> {
        self.primitives.get(type_name).unwrap().clone()
    }

    /// Resolves the type of an expression. Returns a typed AST.
    pub fn resolve_type(
        &mut self,
        expression: &UntypedNode,
    ) -> Result<(Rc<DataType>, TypedNode), String> {
        match expression {
            UntypedNode::Integer(val) => {
                let int_type = self.resolve_primitive("int");

                Ok((int_type.clone(), TypedNode::Integer(int_type, *val)))
            }
            UntypedNode::Float(val) => {
                let float_type = self.resolve_primitive("float");

                Ok((float_type.clone(), TypedNode::Float(float_type, *val)))
            }
            UntypedNode::Str(val) => {
                let str_type = self.resolve_primitive("string");

                Ok((str_type.clone(), TypedNode::Str(str_type, val.clone())))
            }
            UntypedNode::Boolean(val) => {
                let bool_type = self.resolve_primitive("bool");

                Ok((bool_type.clone(), TypedNode::Boolean(bool_type, *val)))
            }
            UntypedNode::Group(inner) => self.resolve_type(inner),
            UntypedNode::Variable(name) => {
                let var_type = self.get_variable(name)?;

                Ok((
                    var_type.clone(),
                    TypedNode::Variable(var_type, name.clone()),
                ))
            }

            UntypedNode::Unary(expr, operator) => {
                let (expr_type, expr_node) = self.resolve_type(expr)?;

                if check_unary(operator, expr_type.clone()) {
                    Ok((
                        expr_type.clone(),
                        TypedNode::Unary(expr_type, Box::new(expr_node), operator.clone()),
                    ))
                } else {
                    Err(format!(
                        "Cannot apply unary operator {operator} to expression of type {}.",
                        *expr_type
                    ))
                }
            }
            UntypedNode::Binary(left_expr, operator, right_expr) => {
                let (left_type, left_node) = self.resolve_type(left_expr)?;
                let (right_type, right_node) = self.resolve_type(right_expr)?;

                if check_binary(operator, left_type.clone(), right_type) {
                    // Type of binary operation depends on the operator.
                    match operator {
                        BinaryOperator::Add
                        | BinaryOperator::Sub
                        | BinaryOperator::Mult
                        | BinaryOperator::Div => Ok((
                            left_type.clone(),
                            TypedNode::Binary(
                                left_type,
                                Box::new(left_node),
                                operator.clone(),
                                Box::new(right_node),
                            ),
                        )),

                        BinaryOperator::Greater
                        | BinaryOperator::GreaterEq
                        | BinaryOperator::Less
                        | BinaryOperator::LessEq
                        | BinaryOperator::Equality
                        | BinaryOperator::NotEqual => {
                            let bool_type = self.primitives.get("bool").unwrap().clone();

                            Ok((
                                bool_type.clone(),
                                TypedNode::Binary(
                                    bool_type,
                                    Box::new(left_node),
                                    operator.clone(),
                                    Box::new(right_node),
                                ),
                            ))
                        }
                    }
                } else {
                    Err("Binary operator not compatible with types.".to_string())
                }
            }
            UntypedNode::Logical(left_expr, operator, right_expr) => {
                let (left_type, left_node) = self.resolve_type(left_expr)?;
                let (right_type, right_node) = self.resolve_type(right_expr)?;

                if check_logical(operator, left_type, right_type) {
                    let bool_type = self.primitives.get("bool").unwrap().clone();

                    Ok((
                        bool_type.clone(),
                        TypedNode::Logical(
                            bool_type,
                            Box::new(left_node),
                            operator.clone(),
                            Box::new(right_node),
                        ),
                    ))
                } else {
                    Err("Logical operation must be between two boolean conditions.".to_string())
                }
            }

            UntypedNode::If(condition, then_expr, else_expr) => {
                let (condition_type, condition_node) = self.resolve_type(condition)?;

                if *condition_type == DataType::Boolean {
                    let (then_type, then_node) = self.resolve_type(then_expr)?;
                    let (else_type, else_node) = self.resolve_type(else_expr)?;

                    if *then_type == *else_type {
                        Ok((
                            then_type.clone(),
                            TypedNode::If(
                                then_type,
                                Box::new(condition_node),
                                Box::new(then_node),
                                Box::new(else_node),
                            ),
                        ))
                    } else {
                        Err(format!("Branches in 'if' statement must have the same type! Got branches with type {} and {}.", *then_type, *else_type))
                    }
                } else {
                    Err(format!("Expected condition in 'if' statement to be of type boolean, but was {} instead.", *condition_type))
                }
            }

            UntypedNode::Let(var_name, var_ast_type, var_value, body, is_recursive) => {
                let var_type = match var_ast_type {
                    // A type is given.
                    Some(ast_type) => self.convert_ast_type(ast_type)?,

                    // No type is given, infer it based on value / type signature.
                    None => match &**var_value {
                        // Assume that the type of the variable is the same as the function's type
                        // signature.
                        UntypedNode::Function(_, func_type, func_params, _) => {
                            let mut parameter_types = Vec::new();

                            // Resolve types & add parameters to scope.
                            for (_, param_type) in func_params {
                                let arg_type = self.convert_ast_type(param_type)?;
                                parameter_types.push(arg_type.clone());
                            }

                            Rc::new(DataType::Function(
                                parameter_types,
                                self.convert_ast_type(func_type)?,
                            ))
                        }

                        // Not a function, just resolve the type.
                        _ => self.resolve_type(var_value)?.0,
                    },
                };

                // If recursive, it needs to have access to itself. So, add it to the stack
                // before evaluation.
                if *is_recursive {
                    self.add_variable(var_name, var_type.clone())?;
                }

                let (value_type, value_node) = self.resolve_type(var_value)?;

                if *var_type == *value_type {
                    // If recursive, the definition has already been added.
                    if !is_recursive {
                        self.add_variable(var_name, var_type.clone())?;
                    }

                    let (body_type, body_node) = self.resolve_type(body)?;
                    self.remove_variable(var_name.clone())?;

                    Ok((
                        body_type,
                        TypedNode::Let(
                            var_name.clone(),
                            var_type,
                            Box::new(value_node),
                            Box::new(body_node),
                            *is_recursive,
                        ),
                    ))
                } else {
                    Err(format!("Can't define variables with incompatible types! Variable is defined as type {}, but you're assigning it to a value of type {}.", *var_type, *value_type))
                }
            }

            UntypedNode::Function(_, ast_return_type, ast_argument_types, body) => {
                let mut argument_types = Vec::new();
                let mut typed_arguments = Vec::new();

                // Resolve types & add parameters to scope.
                for (arg_name, arg_ast_type) in ast_argument_types {
                    let arg_type = self.convert_ast_type(arg_ast_type)?;

                    argument_types.push(arg_type.clone());
                    typed_arguments.push((arg_name.clone(), arg_type.clone()));

                    self.add_variable(arg_name, arg_type)?;
                }

                let (body_type, body_node) = self.resolve_type(body)?;
                let return_type = self.convert_ast_type(ast_return_type)?;

                // Since the expression has been evaluated, pop parameters from scope.
                for (arg_name, _) in ast_argument_types {
                    self.remove_variable(arg_name.clone())?;
                }

                if *return_type == *body_type {
                    let func_type =
                        Rc::new(DataType::Function(argument_types, return_type.clone()));

                    Ok((
                        func_type.clone(),
                        TypedNode::Function(
                            func_type,
                            return_type,
                            typed_arguments,
                            Box::new(body_node),
                        ),
                    ))
                } else {
                    Err("Function return type and actual type returned do not match.".to_string())
                }
            }
            UntypedNode::FunctionCall(parent, parameters) => {
                // Verify that the parameter & argument types match.
                let (func_type, func_node) = self.resolve_type(parent)?;

                match &*func_type {
                    DataType::Function(arguments, return_type) => {
                        let mut typed_arguments = Vec::new();

                        // Check that the types of the expressions match the expected type.
                        for (target_type, parameter) in arguments.iter().zip(parameters) {
                            let (resolved_type, resolved_node) = self.resolve_type(parameter)?;

                            if *target_type != resolved_type {
                                return Err(format!(
                                    "Invalid parameter type, expected {target_type} but got {resolved_type}."
                                ));
                            }

                            typed_arguments.push(resolved_node);
                        }

                        Ok((
                            return_type.clone(),
                            TypedNode::FunctionCall(
                                return_type.clone(),
                                Box::new(func_node),
                                typed_arguments,
                            ),
                        ))
                    }

                    _ => Err("Cannot call non-function.".to_string()),
                }
            }

            UntypedNode::RecordInstance(name, fields) => {
                let record_type = self.get_record(name)?;
                let mut field_types = BTreeMap::new();
                let mut field_values = Vec::new();

                for (field_name, field_ast_type) in fields {
                    let (field_type, field_value) = self.resolve_type(field_ast_type)?;
                    field_types.insert(field_name.clone(), field_type);
                    field_values.push((field_name.clone(), field_value));
                }

                if DataType::Record(name.clone(), field_types) == *record_type {
                    Ok((
                        record_type,
                        TypedNode::RecordInstance(name.clone(), field_values),
                    ))
                } else {
                    Err("Records don't match.".to_string())
                }
            }
            UntypedNode::RecordDeclaration(name, fields, body) => {
                self.add_record(name, fields)?;
                self.resolve_type(body)
            }

            UntypedNode::AnonymousRecord(fields) => {
                let mut field_types = BTreeMap::new();
                let mut field_values: Vec<(String, TypedNode)> = Vec::new();

                for (field_name, field_ast_type) in fields {
                    let (field_type, field_value) = self.resolve_type(field_ast_type)?;

                    field_types.insert(field_name.clone(), field_type);
                    field_values.push((field_name.clone(), field_value));
                }

                let fresh_name = self.add_anonymous_record(field_types.clone())?;

                Ok((
                    Rc::new(DataType::Record(fresh_name.clone(), field_types)),
                    TypedNode::RecordInstance(fresh_name, field_values),
                ))
            }

            UntypedNode::Get(field, parent) => {
                let (get_type, get_node) = self.resolve_type(parent)?;

                match &*get_type {
                    DataType::Record(name, fields) => match fields.get(field) {
                        Some(field_type) => Ok((
                            field_type.clone(),
                            TypedNode::Get(field_type.clone(), name.clone(), Box::new(get_node)),
                        )),

                        None => Err(format!("Can't find field {field} in record of type {name}")),
                    },

                    invalid_type => Err(format!("Can't access fields from type {invalid_type}.")),
                }
            }

            UntypedNode::Extern(name, ast_type, body) => {
                let extern_type = self.convert_ast_type(ast_type)?;
                self.add_variable(name, extern_type.clone())?;

                let (body_type, body_node) = self.resolve_type(body)?;

                Ok((
                    body_type,
                    TypedNode::Extern(name.clone(), extern_type, Box::new(body_node)),
                ))
            }
        }
    }
}

impl Default for Typechecker {
    fn default() -> Self {
        Self::new()
    }
}
