/// Transforms pattern matching expressions into simple matches on primitives.
use std::collections::HashMap;
use std::rc::Rc;

use crate::postprocess::pattern::compiler::PatternCompiler;
use crate::postprocess::PostprocessPhase;

use crate::ast::typed::data_type::DataType;
use crate::ast::typed::typed_node::TypedNode;

use super::pattern::Pattern;

pub struct PatternPhase<'a> {
    enums: &'a HashMap<String, Rc<DataType>>,
}

impl<'a> PatternPhase<'a> {
    pub fn new(enums: &'a HashMap<String, Rc<DataType>>) -> Self {
        PatternPhase { enums }
    }
}

impl<'a> PostprocessPhase for PatternPhase<'a> {
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
            | TypedNode::RecordDeclaration(..) => root.clone(),

            TypedNode::Group(_, body) => self.transform(body),

            TypedNode::Extern(extern_name, extern_type, body) => TypedNode::Extern(
                extern_name.clone(),
                extern_type.clone(),
                Box::new(self.transform(body)),
            ),

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

            TypedNode::CaseOf(_, expr, arms) => {
                let compiler = PatternCompiler::new(self.enums);

                // TODO: show error message here at runtime.

                // Construct call to 'exit(1)'
                let integer_type = Rc::new(DataType::Integer);
                let exit_type =
                    DataType::Function(vec![integer_type.clone()], integer_type.clone());

                let default = TypedNode::FunctionCall(
                    integer_type.clone(),
                    Box::new(TypedNode::Variable(
                        Rc::new(exit_type),
                        String::from("panic"),
                    )),
                    vec![TypedNode::Integer(integer_type, 1)],
                );

                let patterns = arms
                    .iter()
                    .map(|(arm_cond, arm_body, _)| {
                        (vec![Pattern::new(arm_cond)], self.transform(arm_body))
                    })
                    .collect::<Vec<(Vec<Pattern>, TypedNode)>>();

                compiler.transform(&[(**expr).clone()], &patterns, default)
            }
        }
    }
}
