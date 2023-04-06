#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, HashMap};
    use std::rc::Rc;

    use crate::{
        ast::typed::{data_type::DataType, typed_node::TypedNode},
        postprocess::pattern::{compiler::PatternCompiler, Pattern},
    };

    #[test]
    fn all_leading_variable() {
        /*
            case var_a, var_b of
                a, _ -> var_c
                b, _ -> var_d,
                _, _ -> default
            end

            a & b both match.
        */

        let node_type = Rc::new(DataType::Integer);
        let exprs = &[
            TypedNode::Variable(node_type.clone(), String::from("var_a")),
            TypedNode::Variable(node_type.clone(), String::from("var_b")),
        ];

        let patterns = &[
            (
                vec![Pattern::Variable(String::from("a")), Pattern::Wildcard],
                TypedNode::Variable(node_type.clone(), String::from("var_c")),
            ),
            (
                vec![Pattern::Variable(String::from("b")), Pattern::Wildcard],
                TypedNode::Variable(node_type.clone(), String::from("var_d")),
            ),
        ];

        let enums = HashMap::new();
        let compiler = PatternCompiler::new(&enums);
        let result = compiler.transform(exprs, patterns, TypedNode::Integer(node_type.clone(), 0));

        println!("{:?}", result);
    }

    #[test]
    fn leading_constructor() {
        /*
         *   case list of
         *       Cons(id, Cons(id2, None)) -> case_a
         *       Cons(wildcard1, wildcard2) -> case_b
         *       None -> case_c
         *       rest -> case_d
         *   end
         *
         *   In this example, the first set of 'Cons' will reduce to just the initial case, since 'id'
         *   will capture everything before the wildcard does.
         * */
        let node_type = Rc::new(DataType::Integer);
        let mut list_variants = BTreeMap::new();
        list_variants.insert(String::from("None"), vec![]);
        list_variants.insert(
            String::from("Cons"),
            vec![
                node_type.clone(),
                Rc::new(DataType::NamedReference(String::from("List"))),
            ],
        );

        let list_type = Rc::new(DataType::Enum(String::from("List"), list_variants));

        let exprs = &[TypedNode::Variable(list_type.clone(), String::from("list"))];

        let patterns = &[
            (
                vec![Pattern::Constructor(
                    String::from("Cons"),
                    vec![
                        Pattern::Variable(String::from("id")),
                        Pattern::Constructor(
                            String::from("Cons"),
                            vec![
                                Pattern::Variable(String::from("id2")),
                                Pattern::Constructor(String::from("None"), vec![]),
                            ],
                        ),
                    ],
                )],
                TypedNode::Variable(node_type.clone(), String::from("case_a")),
            ),
            (
                vec![Pattern::Constructor(
                    String::from("Cons"),
                    vec![
                        Pattern::Variable(String::from("wildcard1")),
                        Pattern::Variable(String::from("wildcard2")),
                    ],
                )],
                TypedNode::Variable(node_type.clone(), String::from("case_b")),
            ),
            (
                vec![Pattern::Constructor(String::from("None"), vec![])],
                TypedNode::Variable(node_type.clone(), String::from("case_c")),
            ),
            (
                vec![Pattern::Variable(String::from("rest"))],
                TypedNode::Variable(node_type.clone(), String::from("case_d")),
            ),
        ];

        let mut enums = HashMap::new();
        enums.insert(String::from("List"), list_type);

        let compiler = PatternCompiler::new(&enums);
        let result = compiler.transform(exprs, patterns, TypedNode::Integer(node_type.clone(), 0));

        println!("{:?}", result);
    }
}
