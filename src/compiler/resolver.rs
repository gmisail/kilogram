/*
 * Resolves C types from native Kilogram types
 * */

use std::rc::Rc;

use crate::typed::data_type::DataType;

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

        DataType::SelfReference => panic!("Self-referencing type should not be used directly during compilation."),

        DataType::Enum(name, _) => format!("{name}*"),

        DataType::Function(_, _) => "KiloFunction*".to_string(),
        DataType::Record(name, _) => format!("{name}*"),
    }
}
