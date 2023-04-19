use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum AstType {
    Base(String),                  // string, int, ...
    Generic(String, Vec<AstType>), // array<string>, list<int>, ...
    Function(Vec<Box<AstType>>, Box<AstType>),
    Record(Vec<(String, AstType)>),
}

impl Display for AstType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AstType::Base(name) => format!("(AstType, name: {name})"),
                AstType::Generic(name, sub_types) => format!(
                    "(GenericType, name: {name}, sub_types: [{}])",
                    sub_types
                        .iter()
                        .map(|sub_type| format!("{sub_type}"))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                AstType::Function(argument_types, return_type) => {
                    let arg_type_list: Vec<String> =
                        argument_types.iter().map(|arg| arg.to_string()).collect();

                    format!(
                        "(FunctionType, arguments: [{}], return: {return_type})",
                        arg_type_list.join(", ")
                    )
                }
                AstType::Record(fields) => {
                    let field_list: Vec<String> = fields
                        .iter()
                        .map(|(field_name, field_type)| format!("{field_name}: {field_type}"))
                        .collect();

                    format!("(Record {})", field_list.join(", "))
                }
            }
        )
    }
}
