use std::fmt::Display;

#[derive(Clone)]
pub enum AstType {
    Base(String),                  // string, int, ...
    Generic(String, Box<AstType>), // array<string>, list<int>, ...
    Function(Vec<Box<AstType>>, Box<AstType>),
}

impl Display for AstType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AstType::Base(name) => format!("(AstType, name: {})", name),
                AstType::Generic(name, sub_type) =>
                    format!("(GenericType, name: {}, sub_type: {})", name, sub_type),
                AstType::Function(argument_types, return_type) => {
                    let arg_type_list: Vec<String> =
                        argument_types.iter().map(|arg| arg.to_string()).collect();

                    format!(
                        "(FunctionType, arguments: [{}], return: {})",
                        arg_type_list.join(", "),
                        return_type
                    )
                }
            }
        )
    }
}