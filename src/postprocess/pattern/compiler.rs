use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::typed::data_type::DataType;
use crate::ast::typed::typed_node::TypedNode;
use crate::fresh::generator::fresh_variable;

use super::clause::{Clause, Test};
//use super::substitute::{substitute, substitute_all};
use super::Pattern;

pub struct PatternCompiler<'c> {
    enums: &'c HashMap<String, Rc<DataType>>,
    variants: &'c HashMap<String, Rc<DataType>>,
}

impl<'c> PatternCompiler<'c> {
    pub fn new(
        enums: &'c HashMap<String, Rc<DataType>>,
        variants: &'c HashMap<String, Rc<DataType>>,
    ) -> Self {
        PatternCompiler { enums, variants }
    }

    /// Moves all bare variables from clause patterns to expressions.
    fn move_variables(&self, mut clauses: Vec<Clause>) -> Vec<Clause> {
        for clause in &mut clauses {
            let (mut vars, constr) = clause
                .tests
                .iter()
                .cloned()
                .partition(|test| matches!(test.pattern, Pattern::Variable(..)));

            // Replace all tests with only the constructors.
            clause.tests = constr;

            // Move variables out of the tests and into the clause body.
            clause.add_variables(&mut vars);
        }

        clauses
    }

    fn group_by_constructor(
        &self,
        constr_name: &String,
        constr_index: usize,
        clauses: Vec<Clause>,
    ) -> (Vec<Clause>, Vec<Clause>) {
        let mut matching_constr = Vec::new();
        let mut remaining_constr = Vec::new();

        // TODO: we're always using the first column, try a different heuristic (closer to the one in the paper)

        let new_clauses = clauses.clone();

        for mut clause in new_clauses {
            // Case 3: Missing constructor, i.e. column does not exist.
            if clause.tests.get(constr_index).is_none() {
                matching_constr.push(clause.clone());
                remaining_constr.push(clause.clone());
            } else {
                // Which column are we pivoting around?
                let Test { variable, pattern } = clause.tests.remove(constr_index);
                let mut has_match = false;

                match pattern {
                    // Case 1: We have a matching Constructor in this clause.
                    Pattern::Constructor(name, arguments) if name == *constr_name => {
                        let constr_type = self
                            .variants
                            .get(&name)
                            .expect("constructor to exist with variant {name}".into());

                        let constr_variant = match &**constr_type {
                            DataType::Enum(_, variants) => {
                                variants.get(&name).expect("variant to exist")
                            }
                            _ => panic!("Expected constructor type to be Enum."),
                        };

                        // For each of the Constructor's arguments, bind it to a
                        // free variable.
                        let mut nested_tests = arguments
                            .iter()
                            .zip(constr_variant)
                            .map(|(arg, arg_type)| Test {
                                variable: TypedNode::Variable(
                                    // TODO: replace this with actual type of argument.
                                    arg_type.clone(),
                                    fresh_variable("var"),
                                ),
                                pattern: arg.clone(),
                            })
                            .collect::<Vec<Test>>();

                        // Add remaining tests in clause.
                        for test in &clause.tests {
                            nested_tests.push(test.clone());
                        }

                        let nested_clause = Clause {
                            tests: nested_tests,
                            body: clause.body.clone(),
                            variables: Vec::new(),
                        };

                        matching_constr.push(nested_clause);
                        has_match = true;
                    }

                    _ => {
                        has_match = false;
                    }
                }

                // Case 2: Constructor does not match.
                if !has_match {
                    remaining_constr.push(clause.clone());
                }
            }
        }

        (matching_constr, remaining_constr)
    }

    /// Transforms a complex match expression into a simplified
    /// matching expression.
    pub fn transform(
        &self,
        _expression: TypedNode,
        clauses: Vec<Clause>,
        default: TypedNode,
    ) -> TypedNode {
        // No clauses => in-exhaustive
        if clauses.is_empty() {
            println!("In-exhaustive pattern match.");
            return default.clone();
        }

        // Move variables to expressions, only Constructor patterns.
        let only_constr = self.move_variables(clauses);

        // If there are no tests to match against, then it will always match. So,
        // return the expression corresponding to the first clause.
        let first_clause = only_constr.first().expect("has first clause");
        if first_clause.tests.is_empty() {
            return first_clause.body.clone();
        }

        // TODO: choose constructor using some heuristic, not just the first one.
        let first_constr = only_constr.first().expect("a leading constructor");
        let first_test = first_constr.tests.first().expect("a first test");
        let Test {
            pattern: first_pattern,
            variable: first_var,
        } = first_test.clone();

        if let Pattern::Constructor(constr_name, _arguments) = first_pattern {
            println!("Matching on: {constr_name}");

            let (matching_constr, remaining_constr) =
                self.group_by_constructor(&constr_name, 0, only_constr);

            println!(
                "MATCHING: {:#?}",
                matching_constr // self.transform(first_var, matching_constr, default.clone())
            );
            println!(
                "REMAINING: {:#?}",
                remaining_constr /*  self.transform(
                                     TypedNode::Variable(Rc::from(DataType::Integer), fresh_variable("b")),
                                     remaining_constr,
                                     default
                                 )*/
            );

            todo!()
        } else {
            panic!("Expected all tests to be a constructor.")
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, HashMap};
    use std::rc::Rc;

    use crate::postprocess::pattern::clause::{Clause, Test};
    use crate::{
        ast::typed::{data_type::DataType, typed_node::TypedNode},
        postprocess::pattern::{compiler::PatternCompiler, Pattern},
    };

    #[test]
    fn leading_constructor() {
        /*
         *   case list of
         *       Cons(id, Cons(id2, Nil)) -> case_a
         *       Cons(wildcard1, wildcard2) -> case_b
         *       Nil -> case_c
         *   end
         *
         * */
        let node_type = Rc::new(DataType::Integer);
        let mut list_variants = BTreeMap::new();
        list_variants.insert(String::from("Nil"), vec![]);
        list_variants.insert(
            String::from("Cons"),
            vec![
                node_type.clone(),
                Rc::new(DataType::NamedReference(String::from("List"))),
            ],
        );

        let list_type = Rc::new(DataType::Enum(String::from("List"), list_variants));

        let exprs = TypedNode::Variable(list_type.clone(), String::from("list"));

        let clauses = vec![
            Clause {
                tests: vec![Test {
                    variable: TypedNode::Variable(node_type.clone(), String::from("a")),
                    pattern: Pattern::Constructor(
                        String::from("Cons"),
                        vec![
                            Pattern::Variable(String::from("id")),
                            Pattern::Constructor(
                                String::from("Cons"),
                                vec![
                                    Pattern::Variable(String::from("id2")),
                                    Pattern::Constructor(String::from("Nil"), vec![]),
                                ],
                            ),
                        ],
                    ),
                }],
                body: TypedNode::Variable(node_type.clone(), String::from("case_a")),
                variables: vec![],
            },
            Clause {
                tests: vec![Test {
                    variable: TypedNode::Variable(node_type.clone(), String::from("b")),
                    pattern: Pattern::Constructor(
                        String::from("Cons"),
                        vec![
                            Pattern::Variable(String::from("wildcard1")),
                            Pattern::Variable(String::from("wildcard2")),
                        ],
                    ),
                }],
                body: TypedNode::Variable(node_type.clone(), String::from("case_b")),
                variables: vec![],
            },
            Clause {
                tests: vec![Test {
                    variable: TypedNode::Variable(node_type.clone(), String::from("c")),
                    pattern: Pattern::Constructor(String::from("Nil"), vec![]),
                }],
                body: TypedNode::Variable(node_type.clone(), String::from("case_c")),
                variables: vec![],
            },
            //     vec![Pattern::Constructor(
            //         String::from("Cons"),
            //         vec![
            //             Pattern::Variable(String::from("id")),
            //             Pattern::Constructor(
            //                 String::from("Cons"),
            //                 vec![
            //                     Pattern::Variable(String::from("id2")),
            //                     Pattern::Constructor(String::from("None"), vec![]),
            //                 ],
            //             ),
            //         ],
            //     )],
            //     TypedNode::Variable(node_type.clone(), String::from("case_a")),
            // ),
            // (
            //     vec![Pattern::Constructor(
            //         String::from("Cons"),
            //         vec![
            //             Pattern::Variable(String::from("wildcard1")),
            //             Pattern::Variable(String::from("wildcard2")),
            //         ],
            //     )],
            //     TypedNode::Variable(node_type.clone(), String::from("case_b")),
            // ),
            // (
            //     vec![Pattern::Constructor(String::from("None"), vec![])],
            //     TypedNode::Variable(node_type.clone(), String::from("case_c")),
            // ),
            // (
            //     vec![Pattern::Variable(String::from("rest"))],
            //     TypedNode::Variable(node_type.clone(), String::from("case_d")),
            // ),
        ];

        let mut enums = HashMap::new();
        enums.insert(String::from("List"), list_type.clone());

        let mut variants = HashMap::new();
        variants.insert("Cons".into(), list_type.clone());
        variants.insert("Nil".into(), list_type);

        let compiler = PatternCompiler::new(&enums, &variants);
        let result = compiler.transform(exprs, clauses, TypedNode::Integer(node_type.clone(), 0));

        println!("{:?}", result);
    }
}
