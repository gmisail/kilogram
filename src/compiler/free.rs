/*
* Utility for finding free variables within an expression.
* */

use std::{collections::HashMap, rc::Rc};

use crate::ast::typed::{data_type::DataType, typed_node::TypedNode};

pub fn find_free(node: &TypedNode) -> HashMap<String, Rc<DataType>> {
    match node {
        TypedNode::Integer(..)
        | TypedNode::Float(..)
        | TypedNode::Str(..)
        | TypedNode::Boolean(..)
        | TypedNode::RecordInstance(..) => HashMap::new(),

        TypedNode::Variable(var_type, name) => {
            let mut env = HashMap::new();
            env.insert(name.clone(), var_type.clone());
            env
        }

        TypedNode::Get(_, _, root) => find_free(root),

        TypedNode::Group(_, expr) => find_free(expr),

        TypedNode::Unary(_, expr, _) => find_free(expr),
        TypedNode::Binary(_, left, _, right) => {
            let mut left_free = find_free(left);
            left_free.extend(find_free(right));
            left_free
        }
        TypedNode::Logical(_, left, _, right) => {
            let mut left_free = find_free(left);
            left_free.extend(find_free(right));
            left_free
        }

        TypedNode::If(_, if_expr, then_expr, else_expr) => {
            let mut if_free = find_free(if_expr);
            if_free.extend(find_free(then_expr));
            if_free.extend(find_free(else_expr));
            if_free
        }

        TypedNode::CaseOf(_, expr, arms) => {
            let mut env = find_free(expr);

            for (arm_cond, arm_val, arm_unbound) in arms {
                env.extend(find_free(arm_cond));
                env.extend(find_free(arm_val));

                // Remove all free variables that are actually bound.
                env.retain(|key, _| arm_unbound.contains_key(key));
            }

            env
        }

        TypedNode::Let(name, _, value, body, _) => {
            let mut value_free = find_free(value);
            let mut body_free = find_free(body);

            value_free.remove(name);
            body_free.remove(name);

            value_free.extend(body_free);
            value_free
        }

        TypedNode::Extern(name, _, body) => {
            let mut body_free = find_free(body);
            body_free.remove(name);
            body_free
        }

        TypedNode::Function(_, _, arg_types, body) => {
            let mut free = find_free(body);

            for (arg_name, _) in arg_types {
                free.remove(arg_name);
            }

            free
        }

        TypedNode::FunctionCall(_, name, arguments) => {
            let mut free = HashMap::new();
            free.extend(find_free(name));

            for arg in arguments {
                free.extend(find_free(arg));
            }

            free
        }

        TypedNode::RecordDeclaration(_, _, body) => find_free(body),

        TypedNode::EnumInstance(_, _, arguments) => {
            let mut all_free = HashMap::new();

            for argument in arguments {
                all_free.extend(find_free(argument));
            }

            all_free
        }
    }
}
