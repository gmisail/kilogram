use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use crate::fresh::generator::FreshGenerator;
use crate::typed::data_type::DataType;
use crate::typed::typed_node::TypedNode;

use super::Pattern;

type Case = (Vec<Pattern>, TypedNode);

pub struct PatternCompiler<'c> {
    enums: &'c HashMap<String, Rc<DataType>>,
    fresh: FreshGenerator,
}

impl<'c> PatternCompiler<'c> {
    pub fn new(enums: &'c HashMap<String, Rc<DataType>>) -> Self {
        PatternCompiler {
            enums,
            fresh: FreshGenerator::new("pattern"),
        }
    }

    fn is_variable_or_wildcard(&self, pattern: &Pattern) -> bool {
        match pattern {
            Pattern::Variable(..) | Pattern::Wildcard => true,
            _ => false,
        }
    }

    fn has_leading_constructor(&self, patterns: &[(Vec<Pattern>, TypedNode)]) -> bool {
        match patterns.first() {
            Some((case_patterns, _)) => case_patterns
                .first()
                .map_or(false, |pattern| !self.is_variable_or_wildcard(pattern)),
            None => false,
        }
    }

    ///
    /// Given a list of cases, group by the leading constructor (if it has one.)
    ///
    fn group_by_constructor<'a>(
        &'a self,
        constructors: &Vec<&'a Case>,
    ) -> BTreeMap<&String, Vec<&'a Case>> {
        let mut groups = BTreeMap::new();

        for constructor in constructors {
            let (case_patterns, _) = constructor;

            if let Some(Pattern::Constructor(name, _)) = case_patterns.first() {
                groups
                    .entry(name)
                    .or_insert_with(Vec::new)
                    .push(constructor.clone());
            }
        }

        groups
    }

    ///
    /// From a constructor type and variant, generate a list of well-typed, fresh variables.
    ///
    fn generate_fresh_variables_from_constructor(
        &self,
        constructor_type: Rc<DataType>,
        variant_name: &String,
    ) -> (Vec<String>, Vec<TypedNode>) {
        match &*constructor_type {
            DataType::Enum(_, variants) => {
                let variant_types = variants.get(variant_name).unwrap();

                // TODO: For every argument, generate a fresh name.
                let fresh_names: Vec<String> = variant_types
                    .iter()
                    .map(|_| String::from("fresh") /*self.fresh.next()*/)
                    .collect();

                let fresh_variables = variant_types
                    .iter()
                    .zip(fresh_names.clone())
                    .map(|(variant_type, fresh_name)| {
                        TypedNode::Variable(variant_type.clone(), fresh_name)
                    })
                    .collect::<Vec<TypedNode>>();

                (fresh_names, fresh_variables)
            }

            DataType::NamedReference(parent) => {
                // TODO: handle this unwrap properly
                let parent_constr = self.enums.get(parent).unwrap();

                self.generate_fresh_variables_from_constructor(parent_constr.clone(), variant_name)
            }

            _ => panic!("Unrecognized constructor"),
        }
    }

    ///
    /// Simplify a case-tree with leading constructors, optionally followed by variable cases.
    ///
    fn transform_leading_constructors(
        &self,
        expressions: &[TypedNode],
        patterns: &[(Vec<Pattern>, TypedNode)],
        default: &TypedNode,
    ) -> TypedNode {
        // Split patterns into those with and without leading constructors
        let (constructors, variables): (Vec<&Case>, Vec<&Case>) =
            patterns.iter().partition(|(case_patterns, _)| {
                matches!(case_patterns.first(), Some(Pattern::Constructor(..)))
            });

        let constructor_groups = self.group_by_constructor(&constructors);
        let (head_expr, remaining_exprs) = expressions.split_first().unwrap();
        let mut arms = Vec::new();

        for (group_name, group) in constructor_groups {
            let mut pairs = Vec::new();

            for (group_patterns, group_expression) in group {
                let mut child_patterns = group_patterns.clone();
                let leading_constr = child_patterns.remove(0);

                if let Pattern::Constructor(_, params) = leading_constr {
                    let mut unwrapped_params = params.clone();
                    unwrapped_params.extend(child_patterns);

                    pairs.push((unwrapped_params, group_expression.clone()));
                } else {
                    panic!();
                }
            }

            // Create fresh variables from the arguments of the constructor.
            let constructor_type = head_expr.get_type();
            let (fresh_names, mut fresh_vars) = self
                .generate_fresh_variables_from_constructor(constructor_type.clone(), &group_name);

            // Generate a generic constructor that we can match against
            let fresh_pattern =
                TypedNode::EnumInstance(constructor_type, group_name.clone(), fresh_vars.clone());

            // Prepend these fresh variables to the list of expressions
            fresh_vars.extend(Vec::from(remaining_exprs));

            arms.push((
                fresh_pattern,
                self.transform(&fresh_vars, pairs.as_slice(), default.clone()),
                HashMap::new(),
            ));
        }

        // Find the first variable pattern; the first one is the only one that will match.
        if let Some((var_patterns, var_expr)) = &variables.first() {
            let mut child_patterns = var_patterns.clone();
            child_patterns.remove(0);

            let child_exprs = Vec::from(remaining_exprs);

            arms.push((
                var_expr.clone(),
                self.transform(
                    &child_exprs,
                    &[(child_patterns, var_expr.clone())],
                    default.clone(),
                ),
                HashMap::new(),
            ));
        } else {
            // TODO: change this to fresh variable
            arms.push((
                TypedNode::Variable(head_expr.get_type(), String::from("wildcard")),
                default.clone(),
                HashMap::new(),
            ));
        }

        TypedNode::CaseOf(head_expr.get_type(), Box::new(head_expr.clone()), arms)
    }

    ///
    /// Simplify a case-tree where each case has a leading variable.
    ///
    fn transform_leading_variables(
        &self,
        expressions: &[TypedNode],
        patterns: &[(Vec<Pattern>, TypedNode)],
        default: &TypedNode,
    ) -> TypedNode {
        let (head_expr, tail_exprs) = expressions.split_first().unwrap();

        let new_patterns = patterns
            .iter()
            .map(|(case_patterns, case_expr)| {
                if let Some((_, tail)) = case_patterns.split_first() {
                    (tail.to_vec(), case_expr.clone())
                } else {
                    panic!()
                }
            })
            .collect::<Vec<Case>>();

        // TODO: generate fresh name here
        let fresh_var = TypedNode::Variable(head_expr.get_type(), String::from("fresh_name"));

        // Since we only have variables, the first will match; thus, generate a case expression on
        // the expression 'e' with only one arm:
        //
        //    case e of
        //        fresh => ...
        //    end
        //
        TypedNode::CaseOf(
            head_expr.get_type(),
            Box::new(head_expr.clone()),
            vec![(
                fresh_var,
                self.transform(tail_exprs, &new_patterns, default.clone()),
                HashMap::new(),
            )],
        )
    }

    ///
    /// From a list of expressions & patterns, desugar the pattern match such that we only match against primitives.
    ///
    pub fn transform(
        &self,
        expressions: &[TypedNode],
        patterns: &[Case],
        default: TypedNode,
    ) -> TypedNode {
        // Ensure that we:
        //   a) have at least one pattern
        //   b) the number of expressions is equal to the width of the pattern matrix
        assert!(patterns.len() > 0);
        assert!(patterns
            .iter()
            .all(|(case_patterns, _)| { expressions.len() == case_patterns.len() }));

        // Cases:
        // - No more expressions to match against.
        // - Every pattern has a leading wildcard or variable.
        // - There is a series of constructors followed by a list of variables.
        // - There is a mix of constructors and variables.
        if expressions.len() == 0 {
            patterns.first().unwrap().1.clone()
        } else if patterns.iter().all(|(pat, _)| {
            pat.first().map_or(false, |leading_pattern| {
                self.is_variable_or_wildcard(leading_pattern)
            })
        }) {
            self.transform_leading_variables(expressions, patterns, &default)
        } else if self.has_leading_constructor(patterns) {
            self.transform_leading_constructors(expressions, patterns, &default)
        } else {
            // Case 4
            todo!()
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, HashMap};
    use std::rc::Rc;

    use crate::{
        pattern::{compiler::PatternCompiler, Pattern},
        typed::{data_type::DataType, typed_node::TypedNode},
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
