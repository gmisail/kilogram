use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::typed::data_type::DataType;
use crate::ast::typed::enum_type;
use crate::ast::typed::typed_node::TypedNode;
use crate::fresh::generator::fresh_variable;

use super::clause::{Clause, Test};
//use super::substitute::{substitute, substitute_all};
use super::Pattern;

pub type MatchArm = (TypedNode, TypedNode, HashMap<String, Rc<DataType>>);

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

    fn generate_fresh_constructor_arguments(
        &self,
        enum_type: &Rc<DataType>,
        variant_name: &String,
    ) -> (Vec<String>, Vec<TypedNode>) {
        let variant_types = enum_type::get_variant_fields(enum_type.clone(), variant_name)
            .expect("variant to exist with fields");

        let fresh_names = variant_types
            .iter()
            .map(|_| fresh_variable("var"))
            .collect::<Vec<String>>();

        // Consider the following constructor: Cons(a_0, a_1)
        // From this constructor, convert a_0 and a_1 into fresh variables with the same types.
        let fresh_variables = variant_types
            .iter()
            .zip(fresh_names.clone())
            .map(|(variant_type, variant_name)| {
                TypedNode::Variable(variant_type.clone(), variant_name)
            })
            .collect();

        (fresh_names, fresh_variables)
    }

    fn group_by_constructor(
        &self,
        constr_name: &String,
        constr_index: usize,
        clauses: &[Clause],
    ) -> (Vec<Clause>, Vec<Clause>) {
        let mut matching_constr = Vec::new();
        let mut remaining_constr = Vec::new();

        // TODO: we're always using the first column, try a different heuristic (closer to the one in the paper)

        let new_clauses = clauses.to_owned();

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
                            .unwrap_or_else(|| panic!("constructor to exist with variant {name}"));

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
        expression: TypedNode,
        clauses: Vec<Clause>,
        default: TypedNode,
    ) -> TypedNode {
        // No clauses => in-exhaustive
        if clauses.is_empty() {
            println!("In-exhaustive pattern match.");
            return default;
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
        let first_test = first_clause.tests.first().expect("a first test");
        let Test {
            pattern: first_pattern,
            variable: first_var,
        } = first_test.clone();

        if let Pattern::Constructor(constr_name, _arguments) = first_pattern {
            // Split expression into two arms. So:
            //
            //  case a of
            //      C(a_0, ..., C_n) -> <matching_constr>
            //      _ ->                <remaining_constr>
            //  end
            //
            let (matching_constr, remaining_constr) =
                self.group_by_constructor(&constr_name, 0, &only_constr);

            let matching_arm = self.transform(first_var, matching_constr, default.clone());
            let remaining_arm = self.transform(
                TypedNode::Variable(Rc::from(DataType::Integer), fresh_variable("b")),
                remaining_constr,
                default,
            );

            let constr_type = self
                .variants
                .get(&constr_name)
                .unwrap_or_else(|| panic!("variant with name {constr_name}"));

            let (fresh_names, constr_vars) =
                self.generate_fresh_constructor_arguments(constr_type, &constr_name);

            // AST representation of the patterns that we selected to match against
            let matching_pattern = TypedNode::EnumInstance(
                constr_type.clone(),
                constr_name.clone(),
                constr_vars.clone(),
            );

            let wildcard_name = fresh_variable("wildcard");
            let wildcard_pattern = TypedNode::Variable(constr_type.clone(), wildcard_name.clone());

            // Each arm's body has the same type, so just use the type of the first arm.
            TypedNode::CaseOf(
                first_clause.body.get_type(),
                Box::new(expression),
                vec![
                    (
                        matching_pattern,
                        matching_arm,
                        constr_vars
                            .iter()
                            .zip(fresh_names)
                            .map(|(constr_var, var_name)| (var_name.clone(), constr_var.get_type()))
                            .collect(),
                    ),
                    (
                        wildcard_pattern,
                        remaining_arm,
                        [(wildcard_name, constr_type.clone())].into(),
                    ),
                ],
            )
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

        println!("{:#?}", result);
    }
}
