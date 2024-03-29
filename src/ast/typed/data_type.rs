use std::collections::BTreeMap;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Clone, Debug, Hash, Eq)]
pub enum DataType {
    Integer,
    Float,
    Str,
    Boolean,

    TypeParameter(String),

    NamedReference(String),
    Enum(
        String,
        BTreeMap<String, Vec<Rc<DataType>>>,
        Vec<String>,
        BTreeMap<String, Rc<DataType>>,
    ),
    Function(Vec<Rc<DataType>>, Rc<DataType>),
    Record(String, BTreeMap<String, Rc<DataType>>),
}

impl DataType {
    pub fn substitute_type_params(self, _sub_types: Vec<Rc<DataType>>) -> Result<DataType, String> {
        match self {
            DataType::Enum(_name, _variants, _type_params, _type_param_bindings) => {
                todo!()
            }

            DataType::Record(..) => todo!(),

            _ => Ok(self),
        }
    }

    pub fn set_type_params(self, type_param_bindings: BTreeMap<String, Rc<DataType>>) -> DataType {
        match self {
            DataType::Enum(name, variants, type_params, _) => {
                DataType::Enum(name, variants, type_params, type_param_bindings)
            }

            DataType::Record(..) => todo!(),

            _ => self,
        }
    }
}

impl PartialEq for DataType {
    // Check if two types are equivalent.
    fn eq(&self, other: &Self) -> bool {
        match self {
            // If there are subfields (i.e. functions & records) then we
            // must check to see if they're *all* equal.
            DataType::Function(arguments, return_type) => match other {
                DataType::Function(other_arguments, other_return_type) => {
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

            DataType::Record(_, fields) => match other {
                DataType::Record(_, other_fields) => {
                    let is_subset = fields.len() == other_fields.len();
                    let contains_all = fields.keys().all(|field_name| {
                        let contains = other_fields.contains_key(field_name);
                        let types_match = fields.get(field_name) == other_fields.get(field_name);

                        contains && types_match
                    });

                    is_subset && contains_all
                }

                _ => false,
            },

            DataType::Enum(name, _, _, type_params) => {
                if let DataType::Enum(other_name, _, _, other_type_params) = other {
                    // Perform a couple checks:
                    //  - Same name
                    //  - Same number of type parameter bindings
                    //  - Each of the type parameters are bound to the same type.
                    name == other_name
                        && type_params.len() == other_type_params.len()
                        && type_params
                            .keys()
                            .all(|key| type_params.get(key) == other_type_params.get(key))
                } else {
                    false
                }
            }

            DataType::TypeParameter(first_param) => match other {
                DataType::TypeParameter(second_param) => first_param == second_param,

                _ => false,
            },

            // Left must be a primitive type; just compare
            // the enum names since there are no subfields.
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DataType::Integer => "int".to_string(),
                DataType::Float => "float".to_string(),
                DataType::Boolean => "bool".to_string(),
                DataType::Str => "string".to_string(),

                DataType::TypeParameter(type_param) => type_param.into(),

                DataType::Enum(name, _, _, _) => name.clone(),

                DataType::NamedReference(name) => format!("NamedReference({name})"),

                DataType::Function(argument_types, return_type) => {
                    let arg_type_list: Vec<String> = argument_types
                        .iter()
                        .map(|arg| (*arg).to_string())
                        .collect();

                    format!("({}) -> {}", arg_type_list.join(", "), *return_type)
                }

                DataType::Record(name, field_types) => {
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
