use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use crate::ast::typed::data_type::DataType;
use crate::ast::typed::typed_node::TypedNode;
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

    fn group_by_constructor(clauses: Vec<Clause>) -> (Vec<Clause>, Vec<Clause>) {}

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
            let tree = MatchTree {
                constructor: matched_constr,
                matching_group: vec![],
                remaining_group: vec![],
            };

            todo!()
        } else {
            panic!()
        }

        todo!()
    }
}
