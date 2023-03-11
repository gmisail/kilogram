use crate::typed::typed_node::TypedNode;

use super::Pattern;

fn is_variable_or_wildcard(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Variable(..) | Pattern::Wildcard => true,
        _ => false,
    }
}

///
/// From a list of expressions & patterns, create a decision plan for compiling the pattern match.
///
pub fn transform(
    expressions: &[TypedNode],
    patterns: &[(Vec<Pattern>, TypedNode)],
    default: TypedNode,
) -> String {
    if expressions.len() == 0 {
        format!("{:?}", patterns.first().unwrap().1)
    } else if patterns
        .iter()
        .all(|(pat, _)| pat.first().map_or(false, is_variable_or_wildcard))
    {
        let (head_expr, tail_exprs) = expressions.split_first().unwrap();

        let new_patterns = patterns
            .iter()
            .map(|(case_patterns, case_expr)| {
                let (_, tail) = case_patterns.split_first().unwrap();
                (tail.to_vec(), case_expr.clone())
            })
            .collect::<Vec<(Vec<Pattern>, TypedNode)>>();

        format!(
            "case {:?} of\n{}\nend",
            head_expr,
            transform(tail_exprs, &new_patterns, default)
        )
    } else {
        format!("Done.")
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        pattern::{matrix::transform, Pattern},
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
}
