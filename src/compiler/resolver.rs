/*
 * Resolves C types from native Kilogram types
 * */

use std::rc::Rc;

use crate::ast::typed::data_type::DataType;

pub fn get_function_pointer(name: String, internal_type: Rc<DataType>) -> String {
    match &*internal_type {
        DataType::Function(arguments, return_type) => {
            let mut arg_types = arguments
                .iter()
                .map(|arg| get_native_type(arg.clone()))
                .collect::<Vec<String>>();

            // If a function includes an environment variable, expect a void* representing the
            // environment object.
            arg_types.push("void*".to_string());

            format!(
                "{} (*)({})",
                get_native_type(return_type.clone()),
                arg_types.join(", ")
            )
        }
        _ => format!("{} {name}", get_native_type(internal_type)),
    }
}

pub fn get_native_type(internal_type: Rc<DataType>) -> String {
    match &*internal_type {
        DataType::Integer => "int".to_string(),
        DataType::Float => "float".to_string(),
        DataType::Str => "KiloString*".to_string(),
        DataType::Boolean => "bool".to_string(),

        DataType::TypeParameter(_) => "void*".into(), //todo!("type not supported: {}", type_param),

        DataType::NamedReference(name) => {
            format!("{name}*")
        }

        DataType::Enum(name, ..) => format!("{name}*"),

        DataType::Function(_, _) => "KiloFunction*".to_string(),
        DataType::Record(name, _) => format!("{name}*"),
    }
}

/// In case we want to use the native type as a name (say, in a function name), we need
/// to remove "illegal" characters like '*'
pub fn get_native_type_as_name(native_type: &str) -> String {
    native_type.replace('*', "_ptr")
}
