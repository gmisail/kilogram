/*
 * Resolves C types from native Kilogram types 
 * */

use crate::ast::Type;

// https://stackoverflow.com/questions/10758811/c-syntax-for-functions-returning-function-pointers
pub fn get_native_type(internal_type: &Type) -> String {
    match internal_type {
        Type::Base(name) => name.clone(),
        Type::Generic(_, _) => "TODO".to_string(),
        Type::Function(_, _) => "TODO".to_string()
    }
}
