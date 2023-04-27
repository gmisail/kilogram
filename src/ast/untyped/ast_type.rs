#[derive(Clone, Debug, Eq, Ord, Hash, PartialOrd, PartialEq)]
pub enum AstType {
    Base(String),                  // string, int, ...
    Generic(String, Vec<AstType>), // array<string>, list<int>, ...
    Function(Vec<Box<AstType>>, Box<AstType>),
    Record(Vec<(String, AstType)>),
}

impl AstType {
    pub fn substitute_type_parameter(&self, param: &String, new_type: &AstType) -> AstType {
        match self {
            AstType::Base(name) if *name == *param => new_type.clone(),

            AstType::Base(_) => self.clone(),

            AstType::Generic(name, sub_types) => AstType::Generic(
                name.clone(),
                sub_types
                    .iter()
                    .map(|sub_type| sub_type.substitute_type_parameter(param, new_type))
                    .collect(),
            ),

            AstType::Function(arguments, return_type) => AstType::Function(
                arguments
                    .iter()
                    .map(|argument| {
                        Box::new((*argument).substitute_type_parameter(param, new_type))
                    })
                    .collect(),
                Box::new(return_type.substitute_type_parameter(param, new_type)),
            ),
            AstType::Record(fields) => AstType::Record(
                fields
                    .iter()
                    .map(|(field_name, field_type)| {
                        (
                            field_name.clone(),
                            field_type.substitute_type_parameter(param, new_type),
                        )
                    })
                    .collect(),
            ),
        }
    }

    pub fn convert_generic_to_concrete(&self) -> AstType {
        match self {
            AstType::Base(_) => self.clone(),

            AstType::Generic(name, sub_types) => {
                let concrete = AstType::Generic(
                    name.clone(),
                    sub_types
                        .iter()
                        .map(|sub_type| sub_type.convert_generic_to_concrete())
                        .collect(),
                );

                AstType::Base(concrete.to_string())
            }

            AstType::Function(arguments, return_type) => AstType::Function(
                arguments
                    .iter()
                    .map(|argument| Box::new(argument.convert_generic_to_concrete()))
                    .collect(),
                Box::new(return_type.convert_generic_to_concrete()),
            ),

            AstType::Record(fields) => AstType::Record(
                fields
                    .iter()
                    .map(|(field_name, field_type)| {
                        (field_name.clone(), field_type.convert_generic_to_concrete())
                    })
                    .collect(),
            ),
        }
    }
}

impl ToString for AstType {
    fn to_string(&self) -> String {
        match self {
            AstType::Base(name) => name.clone(),

            AstType::Generic(name, sub_types) => {
                format!(
                    "{name}_{}",
                    sub_types
                        .iter()
                        .map(|sub_type| sub_type.to_string())
                        .collect::<Vec<String>>()
                        .join("_")
                )
            }

            AstType::Function(arguments, return_type) => {
                format!(
                    "fn_{}_ret_{}",
                    arguments
                        .iter()
                        .map(|argument| argument.to_string())
                        .collect::<Vec<String>>()
                        .join("_"),
                    return_type.to_string()
                )
            }

            AstType::Record(fields) => {
                format!(
                    "rec_{}",
                    fields
                        .iter()
                        .map(|(field_name, field_type)| format!(
                            "{field_name}_{}",
                            field_type.to_string()
                        ))
                        .collect::<Vec<String>>()
                        .join("_")
                )
            }
        }
    }
}
