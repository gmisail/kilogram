use std::fmt::Display;
use std::{collections::HashMap, rc::Rc};

#[derive(Clone)]
pub enum Type {
    Integer,
    Float,
    Str,
    Boolean,

    // Only allow one allocation per type and simply create copies
    // of that immutable memory address.
    Function(Vec<Rc<Type>>, Rc<Type>),
    Record(String, HashMap<String, Rc<Type>>),
}

impl PartialEq for Type {
    // Check if two types are equivalent.
    fn eq(&self, other: &Self) -> bool {
        match self {
            // If there are subfields (i.e. functions & records) then we
            // must check to see if they're *all* equal.
            Type::Function(arguments, return_type) => match other {
                Type::Function(other_arguments, other_return_type) => {
                    // Function types are equivalent under the following conditions:
                    //  1. same # of arguments
                    //  2. return types are equal
                    //  3. corresponding arguments have the same types
                    arguments.len() == other_arguments.len()
                        && **return_type == **other_return_type
                        && arguments
                            .iter()
                            .zip(other_arguments)
                            .all(|(type_a, type_b)| **type_a == **type_b)
                }

                // Other type not a function? Must not be equal.
                _ => false,
            },

            Type::Record(_, fields) => match other {
                // Equal if fields is a subset of other_fields. Record 'B' can be used in place of record 'A'
                // as long as 'B' has all of the fields in 'A'. However, that is not to say that they are
                // equivalent; that is only true iff all fields in 'A' are in 'B' and vice-versa.
                Type::Record(_, other_fields) => {
                    fields.len() <= other_fields.len()
                        && fields.keys().all(|field_name| {
                            other_fields.contains_key(field_name)
                                && fields.get(field_name) == other_fields.get(field_name)
                        })
                }

                _ => false,
            },

            // Left must be a primitive type; just compare
            // the enum names since there are no subfields.
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Integer => "int".to_string(),
                Type::Float => "float".to_string(),
                Type::Boolean => "bool".to_string(),
                Type::Str => "string".to_string(),

                Type::Function(argument_types, return_type) => {
                    let arg_type_list: Vec<String> = argument_types
                        .iter()
                        .map(|arg| (*arg).to_string())
                        .collect();

                    format!("({}) -> {}", arg_type_list.join(", "), *return_type)
                }

                Type::Record(name, field_types) => {
                    let field_type_list: Vec<String> = field_types
                        .values()
                        .map(|field_type| (*field_type).to_string())
                        .collect();

                    format!("{} {{ {} }}", name, field_type_list.join(", "))
                }
            }
        )
    }
}
