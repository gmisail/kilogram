use std::collections::HashMap;

use crate::ast::typed::typed_node::TypedNode;

/// Substitutes multiple variables.
///
/// * `root`: Tree to apply the substitutions to.
/// * `substitutions`: Mappings of orignal => new names.
pub fn substitute_all(root: &TypedNode, substitutions: &HashMap<String, String>) -> TypedNode {
    match root {
        TypedNode::Integer(..)
        | TypedNode::Float(..)
        | TypedNode::Str(..)
        | TypedNode::Boolean(..)
        | TypedNode::Get(..)
        | TypedNode::RecordDeclaration(..)
        | TypedNode::Extern(..) => root.clone(),

        TypedNode::Variable(var_type, var_name) if substitutions.contains_key(var_name) => {
            let new_name = substitutions.get(var_name).expect("Expected name to be in mapping.");

            TypedNode::Variable(var_type.clone(), new_name.clone())
        }
        TypedNode::Variable(_, _) => root.clone(),

        TypedNode::Group(_, body) => substitute_all(body, substitutions),

        TypedNode::Let(name, var_type, var_value, body, is_recursive) => TypedNode::Let(
            name.clone(),
            var_type.clone(),
            Box::new(substitute_all(var_value, substitutions)),
            Box::new(substitute_all(body, substitutions)),
            *is_recursive,
        ),

        TypedNode::Unary(data_type, value, operator) => TypedNode::Unary(
            data_type.clone(),
            Box::new(substitute_all(value, substitutions)),
            operator.clone(),
        ),

        TypedNode::Binary(data_type, left_value, operator, right_value) => TypedNode::Binary(
            data_type.clone(),
            Box::new(substitute_all(left_value, substitutions)),
            operator.clone(),
            Box::new(substitute_all(right_value, substitutions)),
        ),

        TypedNode::Logical(data_type, left_value, operator, right_value) => TypedNode::Logical(
            data_type.clone(),
            Box::new(substitute_all(left_value, substitutions)),
            operator.clone(),
            Box::new(substitute_all(right_value, substitutions)),
        ),

        TypedNode::If(data_type, if_cond, then_expr, else_expr) => TypedNode::If(
            data_type.clone(),
            Box::new(substitute_all(if_cond, substitutions)),
            Box::new(substitute_all(then_expr, substitutions)),
            Box::new(substitute_all(else_expr, substitutions)),
        ),

        TypedNode::Function(func_type, return_type, arguments, body) => TypedNode::Function(
            func_type.clone(),
            return_type.clone(),
            arguments.clone(),
            Box::new(substitute_all(body, substitutions)),
        ),

        TypedNode::FunctionCall(return_type, func, arguments) => TypedNode::FunctionCall(
            return_type.clone(),
            Box::new(substitute_all(func, substitutions)),
            arguments
                .iter()
                .map(|argument| substitute_all(argument, substitutions))
                .collect(),
        ),

        TypedNode::EnumInstance(enum_type, name, variants) => TypedNode::EnumInstance(
            enum_type.clone(),
            name.clone(),
            variants
                .iter()
                .map(|variant| substitute_all(variant, substitutions))
                .collect(),
        ),

        TypedNode::RecordInstance(name, fields) => TypedNode::RecordInstance(
            name.clone(),
            fields
                .iter()
                .map(|(field_name, field_value)| {
                    (
                        field_name.clone(),
                        substitute_all(field_value, substitutions),
                    )
                })
                .collect(),
        ),

        TypedNode::CaseOf(data_type, expr, arms) => TypedNode::CaseOf(
            data_type.clone(),
            Box::new(substitute_all(expr, substitutions)),
            arms.iter()
                .map(|(arm_cond, arm_expr, unbound)| {
                    (
                        arm_cond.clone(),
                        substitute_all(arm_expr, substitutions),
                        unbound.clone(),
                    )
                })
                .collect(),
        ),
    }
}

/// Substitutes a single variable name for another.
///
/// * `root`: Tree that will be modified.
/// * `original`: Name that will be replaced.
/// * `updated`: Name to replace the original name with.
pub fn substitute(root: &TypedNode, original: &String, updated: &String) -> TypedNode {
    let mut substitutions = HashMap::new();
    substitutions.insert(original.clone(), updated.clone());
    substitute_all(root, &substitutions)
}
