use crate::ast::typed::typed_node::TypedNode;

use super::Pattern;

/// A test represents a match against a single pattern.
/// Example: <variable> is <pattern>d
#[derive(Clone, Debug)]
pub struct Test {
    pub variable: TypedNode,
    pub pattern: Pattern,
}

/// Given a list of tests, create a chain of let-bindings so they're loaded
/// into scope.
pub fn generate_let_block(variables: &[Test], body: TypedNode) -> TypedNode {
    match variables {
        [head, tail @ ..] => {
            if let Pattern::Variable(var_name) = &head.pattern {
                TypedNode::Let(
                    var_name.clone(),
                    head.variable.get_type(),
                    Box::new(head.clone().variable),
                    Box::new(generate_let_block(tail, body)),
                    false,
                )
            } else {
                panic!()
            }
        }

        [] => body,
    }
}

/// A clause represents a list of tests as well as the body of a successful match.
/// Example: [<test>] => <body>
#[derive(Clone, Debug)]
pub struct Clause {
    pub tests: Vec<Test>,
    pub body: TypedNode,
    pub variables: Vec<Test>,
}

impl Clause {
    pub fn add_variables(&mut self, variable_tests: &mut Vec<Test>) {
        self.variables.append(variable_tests);
    }
}
