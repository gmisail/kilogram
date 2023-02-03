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

fn get_builtin(type_name: &String) -> String {
    match type_name.as_str() {
        "int" | "float" | "bool" => type_name.clone(),
        "string" => "KiloString*".to_string(),
        _ => format!("{}*", type_name),
    }
}

pub fn get_native_type(internal_type: &Type) -> String {
    match internal_type {
        Type::Base(name) => get_builtin(name),

        // TODO: add generics
        Type::Generic(_, _) => panic!("Not yet supported."),

        Type::Function(_, _) => get_function_pointer("".to_string(), internal_type),
    }
}
