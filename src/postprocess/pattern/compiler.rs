use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use crate::fresh::generator::fresh_variable;

use crate::ast::typed::data_type::DataType;
use crate::ast::typed::typed_node::TypedNode;

use super::substitute::{substitute, substitute_all};
use super::Pattern;

type Case = (Vec<Pattern>, TypedNode);
pub type MatchArm = (TypedNode, TypedNode, HashMap<String, Rc<DataType>>);

pub struct PatternCompiler<'c> {
    enums: &'c HashMap<String, Rc<DataType>>,
}

impl<'c> PatternCompiler<'c> {
    pub fn new(enums: &'c HashMap<String, Rc<DataType>>) -> Self {
        PatternCompiler { enums }
    }

    fn is_variable_or_wildcard(&self, pattern: &Pattern) -> bool {
        matches!(pattern, Pattern::Variable(..) | Pattern::Wildcard)
    }

    fn is_mixed(&self, patterns: &[Case]) -> bool {
        let mut has_variable = false;

        for (case_pattern, _) in patterns {
            let first_pattern = case_pattern.first().expect("a leading pattern.");

            // If the first pattern is a constructor, check if we've already
            // found a variable. If so, that means we have a mixed pattern. Otherwise,
            // keep checking.
            match first_pattern {
                &Pattern::Constructor(..) => {
                    if has_variable {
                        return true;
                    }
                }

                &Pattern::Variable(..) => {
                    has_variable = true;
                }

                _ => panic!("Unknown pattern type."),
            };
        }

        false
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
                    .push(*constructor);
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

                let fresh_names: Vec<String> = variant_types
                    .iter()
                    .map(|_| fresh_variable("pattern"))
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
                if let Some(parent_constr) = self.enums.get(parent) {
                    self.generate_fresh_variables_from_constructor(
                        parent_constr.clone(),
                        variant_name,
                    )
                } else {
                    panic!("Self-reference to {parent} does not exist.")
                }
            }

            _ => panic!("Unrecognized constructor"),
        }
    }

    fn substitute_constructor_arguments(
        &self,
        patterns: &[Pattern],
        body: &TypedNode,
        fresh_names: &[String],
    ) -> TypedNode {
        // From a list of patterns, find the variables that need to be substituted and what to
        // substitute them with.
        let fresh_mappings = patterns
            .iter()
            .zip(fresh_names)
            .filter(|(pattern, _)| matches!(pattern, Pattern::Variable(..)))
            .map(|(pattern, fresh)| {
                if let Pattern::Variable(var_name) = pattern {
                    (var_name.clone(), fresh.clone())
                } else {
                    panic!()
                }
            })
            .collect::<Vec<(String, String)>>();

        // Convert list of [(old, new)] to a mapping of { old => new }.
        let substitutions: HashMap<String, String> = fresh_mappings.into_iter().collect();

        // Substitute all instances of "old" in tree "body" with variable "new".
        substitute_all(body, &substitutions)
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
            let (free_names, mut fresh_vars) = self
                .generate_fresh_variables_from_constructor(constructor_type.clone(), group_name);

            // Generate a generic constructor that we can match against
            let fresh_pattern =
                TypedNode::EnumInstance(constructor_type, group_name.clone(), fresh_vars.clone());

            // Substitute the free variable if necessary.
            pairs = pairs
                .iter()
                .cloned()
                .map(|(arm_patterns, arm_body)| {
                    (
                        arm_patterns.clone(),
                        self.substitute_constructor_arguments(
                            &arm_patterns,
                            &arm_body,
                            &free_names,
                        ),
                    )
                })
                .collect();

            // Prepend these fresh variables to the list of expressions
            fresh_vars.extend(Vec::from(remaining_exprs));

            let mapped_variables = free_names
                .iter()
                .zip(fresh_vars.clone())
                .map(|(fresh_name, fresh_node)| (fresh_name.clone(), fresh_node.get_type()))
                .collect::<Vec<(_, _)>>();

            arms.push((
                fresh_pattern,
                self.transform(&fresh_vars, pairs.as_slice(), default.clone()),
                mapped_variables.into_iter().collect(),
            ));
        }

        // Find the first variable pattern; the first one is the only one that will match.
        if let Some((var_patterns, var_expr)) = &variables.first() {
            let mut child_patterns = var_patterns.clone();
            child_patterns.remove(0);

            let child_exprs = Vec::from(remaining_exprs);

            let free_name = fresh_variable("pattern");
            let free_type = head_expr.get_type();

            let original_name = match var_patterns.first() {
                Some(Pattern::Variable(pattern_name)) => pattern_name,
                Some(_) | None => panic!("Expected leading variable."),
            };

            let mapped_variables: HashMap<String, Rc<DataType>> =
                [(free_name.clone(), free_type.clone())]
                    .into_iter()
                    .collect();

            arms.push((
                TypedNode::Variable(free_type.clone(), free_name.clone()),
                self.transform(
                    &child_exprs,
                    &[(
                        child_patterns,
                        substitute(var_expr, &original_name, &free_name),
                    )],
                    default.clone(),
                ),
                mapped_variables,
            ));
        } else {
            let wildcard_name = fresh_variable("wildcard");
            let wildcard_type = head_expr.get_type();

            arms.push((
                TypedNode::Variable(wildcard_type.clone(), wildcard_name.clone()),
                default.clone(),
                [(wildcard_name, wildcard_type)].into_iter().collect(),
            ));
        }

        let (_, first_arm_body, _) = arms.first().expect("an arm to exist");

        TypedNode::CaseOf(first_arm_body.get_type(), Box::new(head_expr.clone()), arms)
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
        let (head_expr, tail_exprs) = expressions
            .split_first()
            .expect("Expected first expression to be variable.");
        let head_name = match head_expr {
            TypedNode::Variable(_, head_name) => head_name,
            _ => panic!(),
        };

        // Fresh name to represent the expression.
        let fresh_name = fresh_variable("var");

        let new_patterns = patterns
            .iter()
            .map(|(case_patterns, case_expr)| {
                if let Some((_, tail)) = case_patterns.split_first() {
                    (tail.to_vec(), substitute(case_expr, head_name, &fresh_name))
                } else {
                    panic!()
                }
            })
            .collect::<Vec<Case>>();

        // Since we only have variables, the first will match; thus, generate a case expression on
        // the expression 'e' with only one arm:
        //
        //    case e of
        //        fresh => body
        //    end
        //
        // We can simplify this further by simply introducing 'fresh' as a variable. So, this
        // expression reduces to:
        //
        //    let fresh = e
        //    body
        //
        TypedNode::Let(
            fresh_name,
            head_expr.get_type(),
            Box::new(head_expr.clone()),
            Box::new(self.transform(tail_exprs, &new_patterns, default.clone())),
            false,
        )
    }

    ///
    /// Transforms a match tree with mixed constructors and variables.
    ///
    fn transform_mixed(
        &self,
        expressions: &[TypedNode],
        patterns: &[(Vec<Pattern>, TypedNode)],
        default: &TypedNode,
    ) -> TypedNode {
        todo!()
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
        assert!(!patterns.is_empty());
        assert!(patterns
            .iter()
            .all(|(case_patterns, _)| { expressions.len() == case_patterns.len() }));

        // Cases:
        // - No more expressions to match against.
        // - Every pattern has a leading wildcard or variable.
        // - There is a series of constructors followed by a list of variables.
        // - There is a mix of constructors and variables.
        if expressions.is_empty() {
            patterns.first().unwrap().1.clone()
        } else if patterns.iter().all(|(pat, _)| {
            pat.first().map_or(false, |leading_pattern| {
                self.is_variable_or_wildcard(leading_pattern)
            })
        }) {
            self.transform_leading_variables(expressions, patterns, &default)
        } else if self.is_mixed(patterns) {
            self.transform_mixed(expressions, patterns, &default)
        } else if self.has_leading_constructor(patterns) {
            self.transform_leading_constructors(expressions, patterns, &default)
        } else {
            panic!("Unhandled case in pattern matching compiler.")
        }
    }
}
