use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

mod error;
mod rules;
mod unify;

use crate::ast::operator::BinaryOperator;
use crate::ast::typed::data_type::DataType;
use crate::ast::untyped::untyped_node::UntypedNode;
use crate::typechecker::unify::unify_enum;
use crate::{ast::typed::typed_node::TypedNode, ast::untyped::ast_type::AstType};
use rules::{check_binary, check_logical, check_unary};

pub struct Typechecker {
    pub enums: HashMap<String, Rc<DataType>>,
    pub records: HashMap<String, Rc<DataType>>,

    primitives: HashMap<&'static str, Rc<DataType>>,
    stack: HashMap<String, Rc<DataType>>,
    anonymous_records: Vec<String>,
    enum_variants: HashMap<String, Rc<DataType>>,
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
            enums: HashMap::new(),
            enum_variants: HashMap::new(),
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

    fn remove_variable(&mut self, var_name: &String) -> Result<(), String> {
        match self.stack.remove(var_name) {
            None => Err(format!(
                "Can't remove variable '{var_name}' since it is not defined."
            )),
            Some(_) => Ok(()),
        }
    }

    fn add_enum(
        &mut self,
        name: &String,
        options: &Vec<(String, Vec<Rc<DataType>>)>,
    ) -> Result<(), String> {
        if self.enums.contains_key(name) {
            Err(format!("Enum {name} already defined."))
        } else {
            let mut option_map = BTreeMap::new();

            for (option_name, option_types) in options {
                option_map.insert(option_name.clone(), option_types.clone());
            }

            let enum_type = Rc::new(DataType::Enum(name.clone(), option_map));

            // Push all variants onto the stack.
            for (option_name, _) in options {
                self.enum_variants
                    .insert(option_name.clone(), enum_type.clone());
            }

            self.enums.insert(name.clone(), enum_type);

            Ok(())
        }
    }

    fn get_enum(&mut self, name: &String) -> Result<Rc<DataType>, String> {
        match self.enums.get(name) {
            Some(enum_type) => Ok(enum_type.clone()),
            None => Err(format!("Can't find enum with name '{name}'")),
        }
    }

    fn get_enum_by_variant(&self, variant_name: &String) -> Result<Rc<DataType>, String> {
        match self.enum_variants.get(variant_name) {
            Some(enum_type) => Ok(enum_type.clone()),
            None => Err(format!(
                "Could not find enum associated with variant {variant_name}."
            )),
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

                // All type parameters must start with '
                name if name.starts_with('\'') => {
                    Ok(Rc::new(DataType::TypeParameter(name.to_string())))
                }

                // First, check if it is an Enum type.
                name if self.enums.contains_key(name) => self.get_enum(&name.to_string()),

                // Otherwise, it must be a record.
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

    fn resolve_reference(&mut self, defined_type: Rc<DataType>) -> Result<Rc<DataType>, String> {
        if let DataType::NamedReference(type_name) = &*defined_type {
            if self.enums.contains_key(type_name) {
                self.get_enum(type_name)
            } else {
                self.get_record(type_name)
            }
        } else {
            Ok(defined_type.clone())
        }
    }

    /// Checks if a node is a) an enum, b) if the number of arguments match, and c) if all of the arguments
    /// match the types declared in the enum declaration.
    ///
    /// * `enum_type`: Parent enum type.
    /// * `variant_name`: Name of the variant we're checking.
    /// * `variant_arguments`: Arguments passed to the variant.
    fn check_enum(
        &mut self,
        enum_type: Rc<DataType>,
        variant_name: &String,
        variant_arguments: &[(Rc<DataType>, TypedNode)],
    ) -> Result<(Rc<DataType>, TypedNode), String> {
        if let DataType::Enum(_, enum_variants) = &*enum_type {
            let expected_variant = enum_variants
                .get(variant_name)
                .expect("Expected variant to be in enum.");

            // Check that the number of arguments match.
            if variant_arguments.len() != expected_variant.len() {
                return Err(format!(
                    "Variant {variant_name} accepts {} arguments, but got {}.",
                    expected_variant.len(),
                    variant_arguments.len()
                ));
            }

            let (arg_types, arg_nodes): (Vec<Rc<DataType>>, Vec<TypedNode>) =
                variant_arguments.iter().cloned().unzip();

            // Check that the actual & expected types match.
            for (defined_type, actual_type) in expected_variant.iter().zip(arg_types) {
                let expected_type = self.resolve_reference(defined_type.clone())?;

                if expected_type != actual_type.clone() {
                    return Err(format!("Invalid parameter type in variant {variant_name}: expected {expected_type}, but got {actual_type}."));
                }
            }

            Ok((
                enum_type.clone(),
                TypedNode::EnumInstance(enum_type, variant_name.clone(), arg_nodes),
            ))
        } else {
            panic!("Expected node of type enum.")
        }
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
                match self.get_enum_by_variant(name) {
                    // Variable name is a variant of previously declared enum.
                    Ok(enum_type) => self.check_enum(enum_type, name, &[]),

                    // Variable name is *not* an enum, treat as normal variable.
                    Err(_) => {
                        let var_type = self.get_variable(name)?;

                        Ok((
                            var_type.clone(),
                            TypedNode::Variable(var_type, name.clone()),
                        ))
                    }
                }
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
            UntypedNode::CaseOf(expression, arms) => {
                let (expr_type, expr_node) = self.resolve_type(expression)?;

                let variants = if let DataType::Enum(_, enum_variants) = &*expr_type {
                    enum_variants
                } else {
                    return Err(
                        "Non-enum types in a case..of expression are not yet supported."
                            .to_string(),
                    );
                };

                let mut checked_arms = Vec::new();
                let mut arm_types = Vec::new();

                for (arm_cond, arm_value) in arms {
                    // Find unbound variables in the condition.
                    let unbound_vars = unify_enum(arm_cond, variants)?;
                    let mut resolved_vars = HashMap::new();

                    // Temporarily bind these unbound variables.
                    for (unbound_name, unbound_type) in &unbound_vars {
                        let resolved_type = self.resolve_reference(unbound_type.clone())?;
                        resolved_vars.insert(unbound_name.clone(), resolved_type.clone());

                        self.add_variable(unbound_name, resolved_type)?;
                    }

                    let (cond_type, cond_node) = self.resolve_type(arm_cond)?;
                    let (val_type, val_node) = self.resolve_type(arm_value)?;

                    // The first arm is used as the reference type; that is, we check to make sure that
                    // all arms have the same types as the first.
                    if let Some((first_cond, first_val)) = arm_types.get(0) {
                        if *first_cond != cond_type {
                            return Err("Mismatched types in case..of arms.".to_string());
                        }

                        if *first_val != val_type {
                            return Err(format!("Mismatched types in case..of arms: got {val_type} but expected {first_val}."));
                        }
                    }

                    checked_arms.push((cond_node, val_node, resolved_vars));
                    arm_types.push((cond_type, val_type));

                    // Remove unbound variables from scope.
                    for unbound_name in unbound_vars.keys() {
                        self.remove_variable(unbound_name)?;
                    }
                }

                // The type of a case..of expression is the type that it returns.
                if let Some((_, val_type)) = arm_types.get(0) {
                    Ok((
                        val_type.clone(),
                        TypedNode::CaseOf(val_type.clone(), Box::new(expr_node), checked_arms),
                    ))
                } else {
                    Err("No arms in case..of expression.".to_string())
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
                    self.remove_variable(var_name)?;

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
                    self.remove_variable(arg_name)?;
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
                    Err(format!("Function is expected to return type {return_type} but instead returns {body_type}."))
                }
            }

            UntypedNode::FunctionCall(parent, parameters) => {
                let mut typed_arguments = Vec::new();

                for parameter in parameters {
                    typed_arguments.push(self.resolve_type(parameter)?);
                }

                if let UntypedNode::Variable(name) = &**parent {
                    // Is this a variant? If not, treat this like any other function call.
                    if let Ok(enum_type) = self.get_enum_by_variant(name) {
                        return self.check_enum(enum_type, name, &typed_arguments);
                    }
                }

                // If we're working with a function that has type parameters, we'll need
                // to store what types the type parameters are bound to.
                let mut type_param_bindings: HashMap<String, Rc<DataType>> = HashMap::new();

                // Verify that the parameter & argument types match.
                let (func_type, func_node) = self.resolve_type(parent)?;

                match &*func_type {
                    DataType::Function(arguments, return_type) => {
                        let mut typed_arguments = Vec::new();

                        for (target_type, parameter) in arguments.iter().zip(parameters) {
                            let (resolved_type, resolved_node) = self.resolve_type(parameter)?;

                            // This parameter is generic, check to make sure it's consistent.
                            if let DataType::TypeParameter(type_param) = &**target_type {
                                let bind_result = type_param_bindings
                                    .insert(type_param.clone(), resolved_type.clone());

                                // This type parameter is already bound to a type, let's make sure
                                // that it's consistent, i.e. we're using the same type.
                                if let Some(existing_binding) = bind_result {
                                    if existing_binding != resolved_type {
                                        return Err(format!(
                                            "Cannot substitute type {resolved_type} for type parameter {type_param}, expected type {existing_binding}." 
                                        ));
                                    }
                                }
                            } else if *target_type != resolved_type {
                                return Err(format!(
                                    "Invalid parameter type, expected {target_type} but got {resolved_type}."
                                ));
                            }

                            typed_arguments.push(resolved_node);
                        }

                        // In case the function returns a generic type, fetch what the it is bound
                        // to.
                        let resolved_return_type = if let DataType::TypeParameter(
                            return_type_param,
                        ) = &**return_type
                        {
                            let actual_return_type = match type_param_bindings
                                .get(return_type_param)
                            {
                                Some(actual_return_type) => actual_return_type.clone(),
                                None => return Err(
                                    "Type parameter {return_type_param} is unbound in return type."
                                        .into(),
                                ),
                            };

                            actual_return_type
                        } else {
                            return_type.clone()
                        };

                        Ok((
                            resolved_return_type.clone(),
                            TypedNode::FunctionCall(
                                resolved_return_type,
                                Box::new(func_node),
                                typed_arguments,
                                type_param_bindings,
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
                            TypedNode::Get(field_type.clone(), field.clone(), Box::new(get_node)),
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

            UntypedNode::EnumDeclaration(name, options, body) => {
                let mut typed_variants = Vec::new();

                // Add enum temporarily so that we can reference it in its variants.
                self.add_enum(name, &Vec::new())?;

                // Compute the types assuming that the self-referential enum is defined.
                for (variant_name, variant_types) in options {
                    let mut typed_variant_types = Vec::new();

                    for variant_type in variant_types {
                        let mut resolved_type = self.convert_ast_type(variant_type)?;

                        if let DataType::Enum(enum_name, _) = &*resolved_type {
                            if name == enum_name {
                                // Since we can't refer to ourselves, use a self-referencing type.
                                resolved_type =
                                    Rc::new(DataType::NamedReference(enum_name.clone()));
                            }
                        }

                        typed_variant_types.push(resolved_type);
                    }

                    typed_variants.push((variant_name.clone(), typed_variant_types));
                }

                // Remove the self-referential enum.
                self.enums.remove(name);

                // Update entry with the new definiton.
                self.add_enum(name, &typed_variants)?;
                self.resolve_type(body)
            }

            UntypedNode::List(_) => panic!("expected list to be converted to Cons operators."),
        }
    }
}

impl Default for Typechecker {
    fn default() -> Self {
        Self::new()
    }
}
