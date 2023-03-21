/// Transforms pattern matching expressions into simple matches on primitives.
use super::DesugarPhase;
use crate::pattern::compiler::PatternCompiler;
use crate::typed::typed_node::TypedNode;

pub struct PatternPhase;

impl PostprocessPhase for PatternPhase {
    /// Replace the body of each node with its desugared version. All
    /// nodes with no body are unchanged.
    fn transform(&self, root: &TypedNode) -> TypedNode {
        match root {
            TypedNode::Integer(..)
            | TypedNode::Variable(..)
            | TypedNode::Float(..)
            | TypedNode::Str(..)
            | TypedNode::Boolean(..)
            | TypedNode::Get(..)
            | TypedNode::RecordDeclaration(..)
            | TypedNode::Extern(..) => root.clone(),

            TypedNode::Group(_, body) => self.transform(body),

            TypedNode::Let(name, var_type, var_value, body, is_recursive) => TypedNode::Let(
                name.clone(),
                var_type.clone(),
                Box::new(self.transform(var_value)),
                Box::new(self.transform(body)),
                *is_recursive,
            ),

            TypedNode::Unary(data_type, value, operator) => TypedNode::Unary(
                data_type.clone(),
                Box::new(self.transform(value)),
                operator.clone(),
            ),

            TypedNode::Binary(data_type, left_value, operator, right_value) => TypedNode::Binary(
                data_type.clone(),
                Box::new(self.transform(left_value)),
                operator.clone(),
                Box::new(self.transform(right_value)),
            ),

            TypedNode::Logical(data_type, left_value, operator, right_value) => TypedNode::Logical(
                data_type.clone(),
                Box::new(self.transform(left_value)),
                operator.clone(),
                Box::new(self.transform(right_value)),
            ),

            TypedNode::If(data_type, if_cond, then_expr, else_expr) => TypedNode::If(
                data_type.clone(),
                Box::new(self.transform(if_cond)),
                Box::new(self.transform(then_expr)),
                Box::new(self.transform(else_expr)),
            ),

            TypedNode::Function(func_type, return_type, arguments, body) => TypedNode::Function(
                func_type.clone(),
                return_type.clone(),
                arguments.clone(),
                Box::new(self.transform(body)),
            ),

            TypedNode::FunctionCall(return_type, func, arguments) => TypedNode::FunctionCall(
                return_type.clone(),
                Box::new(self.transform(func)),
                arguments
                    .iter()
                    .map(|argument| self.transform(argument))
                    .collect(),
            ),

            TypedNode::EnumInstance(enum_type, name, variants) => TypedNode::EnumInstance(
                enum_type.clone(),
                name.clone(),
                variants
                    .iter()
                    .map(|variant| self.transform(variant))
                    .collect(),
            ),

            TypedNode::RecordInstance(name, fields) => TypedNode::RecordInstance(
                name.clone(),
                fields
                    .iter()
                    .map(|(field_name, field_value)| {
                        (field_name.clone(), self.transform(field_value))
                    })
                    .collect(),
            ),

            TypedNode::CaseOf(data_type, expr, arms) => {
                // let compiler = PatternCompiler::new(enums)

                todo!()
            }
        }
    }
}
