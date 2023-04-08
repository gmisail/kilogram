use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use crate::ast::typed::data_type::DataType;
use crate::ast::typed::typed_node::TypedNode;
use crate::fresh::generator::fresh_variable;
use crate::postprocess::pattern::tree::{self, MatchTree};

use super::clause::{Clause, Test};
use super::substitute::{substitute, substitute_all};
use super::Pattern;

pub struct PatternCompiler<'c> {
    enums: &'c HashMap<String, Rc<DataType>>,
}

impl<'c> PatternCompiler<'c> {
    pub fn new(enums: &'c HashMap<String, Rc<DataType>>) -> Self {
        PatternCompiler { enums }
    }

    /// Moves all bare variables from clause patterns to expressions.
    fn move_variables(&self, clauses: Vec<Clause>) -> Vec<Clause> {
        for clause in &mut clauses {
            let mut bare_variables = clause
                .tests
                .iter()
                .filter(|test| matches!(test.pattern, Pattern::Variable(..)))
                .cloned()
                .collect::<Vec<Test>>();

            clause.add_variables(&mut bare_variables);
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

        for clause in clauses {
            let mut has_match = false;

            // Case 3: Missing constructor, i.e. column does not exist.
            if clause.tests.get(constr_index).is_none() {
                matching_constr.push(clause.clone());
                remaining_constr.push(clause.clone());
            } else {
                // Which column are we pivoting around?
                let Test { variable, pattern } = clause.tests.remove(constr_index);

                match pattern {
                    // Case 1: We have a matching Constructor in this clause.
                    Pattern::Constructor(name, arguments) if name == *constr_name => {
                        // For each of the Constructor's arguments, bind it to a
                        // free variable.
                        let mut nested_tests = arguments
                            .iter()
                            .map(|argument| Test {
                                variable: TypedNode::Variable(
                                    Rc::from(DataType::Integer),
                                    "temp".into(),
                                ),
                                pattern: Pattern::Variable(fresh_variable("var")),
                            })
                            .collect::<Vec<Test>>();

                        // Add remaining tests in clause.
                        nested_tests.extend(clause.tests);

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

    pub fn transform(&self, clauses: Vec<Clause>, default: TypedNode) -> TypedNode {
        // No clauses => in-exhaustive
        if clauses.is_empty() {
            panic!("In-exhaustive pattern match.");
        }

        // If there are no tests to match against, then it will always match. So,
        // return the expression corresponding to the first clause.
        let first_clause = clauses.first().expect("has first clause");
        if first_clause.tests.is_empty() {
            return first_clause.body;
        }

        // Move variables to expressions, only Constructor patterns.
        let only_constr = self.move_variables(clauses);

        // TODO: choose constructor using some heuristic, not just the first one.
        let matched_constr = only_constr
            .first()
            .expect("a leading constructor")
            .tests
            .first()
            .expect("a first test")
            .pattern;

        if let Pattern::Constructor(constr_name, arguments) = matched_constr {
            let (matching_constr, remaining_constr) =
                self.group_by_constructor(&constr_name, 0, only_constr);

            println!("MATCHING: {:#?}", matched_constr);
            println!("REMAINING: {:#?}", remaining_constr);

            todo!()
        } else {
            panic!()
        }
    }
}
