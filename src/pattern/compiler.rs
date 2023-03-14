use std::collections::BTreeMap;
use std::rc::Rc;

use crate::typed::data_type::DataType;
use crate::typed::typed_node::TypedNode;

use super::Pattern;

type Case = (Vec<Pattern>, TypedNode);

fn is_variable_or_wildcard(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Variable(..) | Pattern::Wildcard => true,
        _ => false,
    }
}

fn has_leading_constructor(patterns: &[(Vec<Pattern>, TypedNode)]) -> bool {
    match patterns.first() {
        Some((case_patterns, _)) => case_patterns
            .first()
            .map_or(false, |pattern| !is_variable_or_wildcard(pattern)),
        None => false,
    }
}

///
/// From a list of expressions & patterns, desugar the pattern match such that we only match against primitives.
///
pub fn transform(expressions: &[TypedNode], patterns: &[Case], default: TypedNode) -> String {
    // Ensure that we:
    //   a) have at least one pattern
    //   b) the number of expressions is equal to the width of the pattern matrix
    assert!(patterns.len() > 0);
    assert!(patterns
        .iter()
        .all(|(case_patterns, _)| { expressions.len() == case_patterns.len() }));

    /*
        Cases:
            - No more expressions to match against.
            - Every pattern has a leading wildcard or variable.
            - There is a series of constructors followed by a list of variables.
            - There is a mix of constructors and variables.
    */
    if expressions.len() == 0 {
        format!("{:?}\n", patterns.first().unwrap().1)
    } else if patterns
        .iter()
        .all(|(pat, _)| pat.first().map_or(false, is_variable_or_wildcard))
    {
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

        format!(
            "case {:?} of\n{}\nend\n",
            head_expr,
            transform(tail_exprs, &new_patterns, default)
        )
    } else if has_leading_constructor(patterns) {
        // Split patterns into those with and without leading constructors
        let (constructors, variables): (Vec<&Case>, Vec<&Case>) =
            patterns.iter().partition(|(case_patterns, _)| {
                if let Some(Pattern::Constructor(..)) = case_patterns.first() {
                    true
                } else {
                    false
                }
            });

        // Organize each of the patterns into buckets, match on the constructor type of the first
        // pattern
        let mut constructor_groups: BTreeMap<&String, Vec<&Case>> = BTreeMap::new();

        for constructor in &constructors {
            let (case_patterns, _) = constructor;

            if let Some(Pattern::Constructor(name, _)) = case_patterns.first() {
                constructor_groups
                    .entry(name)
                    .or_insert_with(Vec::new)
                    .push(constructor);
            }
        }

        let mut buffer = String::new();
        buffer.push_str(&format!("case {:?} of\n", expressions));

        for (group_name, group) in &constructor_groups {
            buffer.push_str(format!("{group_name} -> ").as_str());

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

            // TODO: Prepend n fresh variables to expressions, where n is the number of arguments in the enum constructor
            let node_type = Rc::new(DataType::Integer);
            let mut child_exprs = if group_name.contains("Cons") {
                vec![
                    TypedNode::Variable(node_type.clone(), String::from("a")),
                    TypedNode::Variable(node_type.clone(), String::from("b")),
                ]
            } else {
                Vec::new()
            };

            child_exprs.extend(Vec::from(&expressions[1..]));

            buffer.push_str(&transform(&child_exprs, pairs.as_slice(), default.clone()));
        }

        // Find the first variable pattern; the first one is the only one that will match
        if let Some((var_patterns, var_expr)) = &variables.first() {
            let mut child_patterns = var_patterns.clone();
            let matched_var = child_patterns.remove(0);

            let child_exprs = Vec::from(&expressions[1..]);

            if let Pattern::Variable(matched_name) = matched_var {
                buffer.push_str(format!("{matched_name} -> ").as_str());
            }

            buffer.push_str(&transform(
                &child_exprs,
                &[(child_patterns, var_expr.clone())],
                default.clone(),
            ));
        } else {
            // No variable pattern? Create a catch-all variable that returns the default case.
        }

        buffer.push_str("\nend\n");

        buffer
    } else {
        format!("Done.")
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        pattern::{compiler::transform, Pattern},
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

        println!(
            "{}",
            transform(
                exprs,
                patterns,
                TypedNode::Variable(node_type.clone(), String::from("default"))
            )
        );
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
        let exprs = &[TypedNode::Variable(node_type.clone(), String::from("list"))];

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

        println!(
            "{}",
            transform(
                exprs,
                patterns,
                TypedNode::Variable(node_type.clone(), String::from("default"))
            )
        );
    }
}
