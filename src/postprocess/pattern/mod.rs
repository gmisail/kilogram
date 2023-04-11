mod substitute;

pub mod clause;
pub mod compiler;

use crate::ast::typed::typed_node::TypedNode;

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Constructor(String, Vec<Pattern>),
    Variable(String),
    Wildcard,
}

impl Pattern {
    pub fn new(expr: &TypedNode) -> Pattern {
        match expr {
            TypedNode::Variable(_, variable_name) => Pattern::Variable(variable_name.clone()),
            TypedNode::EnumInstance(_, variant, variant_arguments) => Pattern::Constructor(
                variant.clone(),
                variant_arguments.iter().map(Pattern::new).collect(),
            ),

            _ => panic!("Cannot create pattern from expression."),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::postprocess::pattern::Pattern;

    use crate::ast::typed::data_type::DataType;
    use crate::ast::typed::typed_node::TypedNode;

    #[test]
    fn creates_variable() {
        let node_type = Rc::new(DataType::Integer);
        let tree = TypedNode::Variable(node_type, String::from("my_var"));

        let var_pattern = Pattern::new(&tree);

        match var_pattern {
            Pattern::Variable(found_name) => {
                assert_eq!(found_name, "my_var");
            }

            _ => panic!(),
        }
    }

    #[test]
    fn creates_enum() {
        let node_type = Rc::new(DataType::Integer);

        // my_enum(my_var)
        let tree = TypedNode::EnumInstance(
            node_type.clone(),
            String::from("my_enum"),
            vec![TypedNode::Variable(node_type, String::from("my_var"))],
        );

        let var_pattern = Pattern::new(&tree);

        match var_pattern {
            Pattern::Constructor(name, arguments) if name == "my_enum" => {
                assert_eq!(1, arguments.len());

                if let Some(Pattern::Variable(var_name)) = arguments.get(0) {
                    assert_eq!("my_var", var_name);
                } else {
                    panic!();
                }
            }

            _ => panic!(),
        }
    }
}
