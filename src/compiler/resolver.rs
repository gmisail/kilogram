/*
 * Resolves C types from native Kilogram types
 * */

use std::rc::Rc;

use crate::typed::data_type::DataType;

pub fn get_function_pointer(name: String, internal_type: Rc<DataType>) -> String {
    match &*internal_type {
        DataType::Function(arguments, return_type) => {
            let signature = format!(
                "(*{})({})",
                name,
                arguments
                    .iter()
                    .map(|arg_type| get_native_type(arg_type.clone()))
                    .collect::<Vec<String>>()
                    .join(", ")
            );

            get_function_pointer(signature, return_type.clone())
        }
        _ => format!("{} {}", get_native_type(internal_type), name),
    }
}

pub fn get_native_type(internal_type: Rc<DataType>) -> String {
    match &*internal_type {
        DataType::Integer => "int".to_string(),
        DataType::Float => "float".to_string(),
        DataType::Str => "KiloString*".to_string(),
        DataType::Boolean => "bool".to_string(),

        DataType::Function(_, _) => "KiloFunction*".to_string(),
        DataType::Record(name, _) => format!("{}*", name),
    }
}
