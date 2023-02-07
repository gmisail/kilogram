/*
 * Resolves C types from native Kilogram types
 * */

use crate::ast::ast_type::AstType;

pub fn get_function_pointer(name: String, internal_type: &AstType) -> String {
    match internal_type {
        AstType::Function(arguments, return_type) => {
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

pub fn get_native_type(internal_type: &AstType) -> String {
    match internal_type {
        AstType::Base(name) => get_builtin(name),

        // TODO: add generics
        AstType::Generic(_, _) => panic!("Not yet supported."),

        AstType::Function(_, _) => "KiloFunction*".to_string(),
    }
}
