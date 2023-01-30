/*
 * Resolves C types from native Kilogram types
 * */

use crate::ast::Type;

pub fn get_function_pointer(name: String, internal_type: &Type) -> String {
    match internal_type {
        Type::Function(arguments, return_type) => {
            let signature = format!(
                "(*{})({})",
                name,
                arguments
                    .iter()
                    .map(|arg_type| get_native_type(arg_type))
                    .collect::<Vec<String>>()
                    .join(", ")
            );

            get_function_pointer(signature, return_type)
        }
        _ => format!("{} {}", get_native_type(internal_type), name),
    }
}

pub fn get_native_type(internal_type: &Type) -> String {
    match internal_type {
        Type::Base(name) => name.clone(),
        Type::Generic(_, _) => "TODO".to_string(),
        Type::Function(arguments, return_type) => "FUNC".to_string(),
    }
}
