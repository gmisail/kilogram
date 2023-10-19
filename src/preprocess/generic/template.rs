use crate::ast::untyped::{ast_type::AstType, untyped_node::UntypedNode};

pub trait Template {
    fn substitute(&self, variants: &[AstType], body: UntypedNode) -> UntypedNode;
}

pub fn substitute_all(subject_type: AstType, types: &[(String, AstType)]) -> AstType {
    match types {
        [(type_name, substituted_type), tail @ ..] => {
            let result = subject_type.substitute_type_parameter(type_name, substituted_type);

            substitute_all(result, tail)
        }

        [] => subject_type,
    }
}
