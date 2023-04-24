use std::collections::HashMap;

use crate::ast::untyped::untyped_node::UntypedNode;

use crate::preprocess::generic::template::RecordTemplate;
use crate::preprocess::PreprocessPhase;

pub struct GenericPhase {
    templates: HashMap<String, RecordTemplate>,
}

impl GenericPhase {
    pub fn new() -> Self {
        GenericPhase {
            templates: HashMap::new(),
        }
    }
}

impl PreprocessPhase for GenericPhase {
    /// Replace the body of each node with its desugared version. All
    /// nodes with no body are unchanged.
    fn transform(&mut self, root: &UntypedNode) -> UntypedNode {
        match root {
            UntypedNode::Integer(..)
            | UntypedNode::Variable(..)
            | UntypedNode::Float(..)
            | UntypedNode::Str(..)
            | UntypedNode::Boolean(..)
            | UntypedNode::Get(..) => root.to_owned(),

            UntypedNode::RecordDeclaration(name, fields, type_params, body) => {
                // If we have more than one type parameter, then this is a generic record.
                if !type_params.is_empty() {
                    self.templates.insert(
                        name.clone(),
                        RecordTemplate::new(type_params.clone(), fields.clone()),
                    );

                    self.transform(body)
                } else {
                    root.to_owned()
                }
            }

            UntypedNode::Group(body) => self.transform(body),

            UntypedNode::Extern(extern_name, extern_type, body) => UntypedNode::Extern(
                extern_name.clone(),
                extern_type.clone(),
                Box::new(self.transform(body)),
            ),

            UntypedNode::Let(name, var_type, var_value, body, is_recursive) => UntypedNode::Let(
                name.clone(),
                var_type.clone(),
                Box::new(self.transform(var_value)),
                Box::new(self.transform(body)),
                *is_recursive,
            ),

            UntypedNode::Unary(value, operator) => {
                UntypedNode::Unary(Box::new(self.transform(value)), operator.clone())
            }

            UntypedNode::Binary(left_value, operator, right_value) => UntypedNode::Binary(
                Box::new(self.transform(left_value)),
                operator.clone(),
                Box::new(self.transform(right_value)),
            ),

            UntypedNode::Logical(left_value, operator, right_value) => UntypedNode::Logical(
                Box::new(self.transform(left_value)),
                operator.clone(),
                Box::new(self.transform(right_value)),
            ),

            UntypedNode::If(if_cond, then_expr, else_expr) => UntypedNode::If(
                Box::new(self.transform(if_cond)),
                Box::new(self.transform(then_expr)),
                Box::new(self.transform(else_expr)),
            ),

            UntypedNode::Function(func_type, return_type, arguments, body) => {
                UntypedNode::Function(
                    func_type.clone(),
                    return_type.clone(),
                    arguments.clone(),
                    Box::new(self.transform(body)),
                )
            }

            UntypedNode::FunctionCall(parent, arguments) => UntypedNode::FunctionCall(
                Box::new(self.transform(parent)),
                arguments
                    .iter()
                    .map(|argument| self.transform(argument))
                    .collect(),
            ),

            UntypedNode::RecordInstance(name, type_params, fields) => {
                // TODO: create instance of template if generic

                let new_type = if let Some(template) = self.templates.get(name) {
                    template.substitute(type_params)
                } else {
                    panic!()
                };

                UntypedNode::RecordInstance(
                    name.clone(),
                    type_params.clone(),
                    fields
                        .iter()
                        .map(|(field_name, field_value)| {
                            (field_name.clone(), self.transform(field_value))
                        })
                        .collect(),
                )
            }

            UntypedNode::EnumDeclaration(name, variants, type_params, body) => {
                // TODO: handle generics

                UntypedNode::EnumDeclaration(
                    name.clone(),
                    variants
                        .iter()
                        .map(|(variant_name, _variant_types)| {
                            // TODO: convert generic types to concrete types.
                            (variant_name.clone(), Vec::new())
                        })
                        .collect(),
                    type_params.clone(),
                    Box::new(self.transform(body)),
                )
            }

            UntypedNode::List(elements) => UntypedNode::List(
                elements
                    .iter()
                    .map(|element| self.transform(element))
                    .collect(),
            ),

            UntypedNode::AnonymousRecord(..) => todo!(),

            UntypedNode::CaseOf(_expr, _arms) => {
                todo!("add generic checking to case of")
            }
        }
    }
}
