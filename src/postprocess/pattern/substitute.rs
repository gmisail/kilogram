use crate::ast::typed::typed_node::TypedNode;

/// Substitutes a variable name for another.
///
/// * `root`: Tree that will be modified.
/// * `original`: Name that will be replaced.
/// * `updated`: Name to replace the original name with.
pub fn substitute(root: &TypedNode, original: &String, updated: &String) -> TypedNode {
    println!("replace {original} with {updated}");

    match root {
        TypedNode::Integer(..)
        | TypedNode::Float(..)
        | TypedNode::Str(..)
        | TypedNode::Boolean(..)
        | TypedNode::Get(..)
        | TypedNode::RecordDeclaration(..)
        | TypedNode::Extern(..) => root.clone(),

        TypedNode::Variable(var_type, var_name) if var_name == original => {
            TypedNode::Variable(var_type.clone(), updated.clone())
        }
        TypedNode::Variable(_, _) => root.clone(),

        TypedNode::Group(_, body) => substitute(body, original, updated),

        TypedNode::Let(name, var_type, var_value, body, is_recursive) => TypedNode::Let(
            name.clone(),
            var_type.clone(),
            Box::new(substitute(var_value, original, updated)),
            Box::new(substitute(body, original, updated)),
            *is_recursive,
        ),

        TypedNode::Unary(data_type, value, operator) => TypedNode::Unary(
            data_type.clone(),
            Box::new(substitute(value, original, updated)),
            operator.clone(),
        ),

        TypedNode::Binary(data_type, left_value, operator, right_value) => TypedNode::Binary(
            data_type.clone(),
            Box::new(substitute(left_value, original, updated)),
            operator.clone(),
            Box::new(substitute(right_value, original, updated)),
        ),

        TypedNode::Logical(data_type, left_value, operator, right_value) => TypedNode::Logical(
            data_type.clone(),
            Box::new(substitute(left_value, original, updated)),
            operator.clone(),
            Box::new(substitute(right_value, original, updated)),
        ),

        TypedNode::If(data_type, if_cond, then_expr, else_expr) => TypedNode::If(
            data_type.clone(),
            Box::new(substitute(if_cond, original, updated)),
            Box::new(substitute(then_expr, original, updated)),
            Box::new(substitute(else_expr, original, updated)),
        ),

        TypedNode::Function(func_type, return_type, arguments, body) => TypedNode::Function(
            func_type.clone(),
            return_type.clone(),
            arguments.clone(),
            Box::new(substitute(body, original, updated)),
        ),

        TypedNode::FunctionCall(return_type, func, arguments) => TypedNode::FunctionCall(
            return_type.clone(),
            Box::new(substitute(func, original, updated)),
            arguments
                .iter()
                .map(|argument| substitute(argument, original, updated))
                .collect(),
        ),

        TypedNode::EnumInstance(enum_type, name, variants) => TypedNode::EnumInstance(
            enum_type.clone(),
            name.clone(),
            variants
                .iter()
                .map(|variant| substitute(variant, original, updated))
                .collect(),
        ),

        TypedNode::RecordInstance(name, fields) => TypedNode::RecordInstance(
            name.clone(),
            fields
                .iter()
                .map(|(field_name, field_value)| {
                    (
                        field_name.clone(),
                        substitute(field_value, original, updated),
                    )
                })
                .collect(),
        ),

        TypedNode::CaseOf(data_type, expr, arms) => TypedNode::CaseOf(
            data_type.clone(),
            Box::new(substitute(expr, original, updated)),
            arms.iter()
                .map(|(arm_cond, arm_expr, unbound)| {
                    (
                        arm_cond.clone(),
                        substitute(arm_expr, original, updated),
                        unbound.clone(),
                    )
                })
                .collect(),
        ),
    }
}
