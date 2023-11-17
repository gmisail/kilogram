use crate::ast::untyped::ast_type::AstType;
use crate::ast::untyped::untyped_node::UntypedNode;
use crate::preprocess::generic::enum_pass::template::EnumTemplate;
use crate::preprocess::generic::pass_state::PassState;
use crate::preprocess::generic::phase::ConcretePass;
use crate::preprocess::generic::template::Template;
use std::collections::HashSet;
use tracing::{info, warn};

pub struct EnumPass {
    state: PassState<EnumTemplate>,
    enums: HashSet<String>,
}

impl EnumPass {
    pub fn new() -> Self {
        EnumPass {
            state: PassState::new(),
            enums: HashSet::new(),
        }
    }
}

impl ConcretePass for EnumPass {
    fn register_type(&mut self, name: String, ast_type: AstType) {
        let is_generic = self
            .state
            .get_template(&name)
            .unwrap()
            .is_instance_generic(&ast_type);

        if !is_generic {
            self.state.register_type(name, ast_type);
        }
    }

    fn find_unique_types(&mut self, root: &UntypedNode) {
        match root {
            UntypedNode::Integer(..)
            | UntypedNode::Variable(..)
            | UntypedNode::Float(..)
            | UntypedNode::Str(..)
            | UntypedNode::Boolean(..) => {}

            UntypedNode::Get(_, parent) => {
                self.find_unique_types(parent);
            }

            UntypedNode::Group(body) | UntypedNode::Extern(_, _, body) => {
                self.find_unique_types(body);
            }

            UntypedNode::RecordDeclaration(name, fields, type_params, body) => {
                self.find_unique_types(body);
            }

            UntypedNode::Let(_, var_type, var_value, body, _) => {
                // If type is explicit, check if it contains a generic type.
                if let Some(t) = var_type {
                    self.resolve_generic_type(t);
                }

                self.find_unique_types(var_value);
                self.find_unique_types(body);
            }

            UntypedNode::Unary(value, _) => {
                self.find_unique_types(value);
            }

            UntypedNode::Binary(left_value, _, right_value)
            | UntypedNode::Logical(left_value, _, right_value) => {
                self.find_unique_types(left_value);
                self.find_unique_types(right_value);
            }

            UntypedNode::If(if_cond, then_expr, else_expr) => {
                self.find_unique_types(if_cond);
                self.find_unique_types(then_expr);
                self.find_unique_types(else_expr);
            }

            UntypedNode::Function(return_type, arguments, body) => {
                // Find generic types in both the return type and arguments.
                self.resolve_generic_type(return_type);

                for (_, arg_type) in arguments {
                    self.resolve_generic_type(arg_type);
                }

                self.find_unique_types(body);
            }

            UntypedNode::FunctionCall(parent, arguments) => {
                self.find_unique_types(parent);

                for argument in arguments {
                    self.find_unique_types(argument);
                }
            }

            UntypedNode::FunctionDeclaration(
                _name,
                _type_params,
                return_type,
                func_params,
                func_body,
                body,
            ) => {
                // Find generic types in both the return type and arguments.
                self.resolve_generic_type(return_type);

                for (_, param_type) in func_params {
                    self.resolve_generic_type(param_type);
                }

                self.find_unique_types(func_body);
                self.find_unique_types(body);
            }

            UntypedNode::RecordInstance(name, type_params, fields) => {
                // Recurse through each of the field values and search for generic types.
                for (_, field_value) in fields {
                    self.find_unique_types(field_value);
                }
            }

            UntypedNode::EnumDeclaration(name, variants, type_params, body) => {
                if !type_params.is_empty() {
                    self.enums.insert(name.clone());

                    self.state.register_template(
                        name.clone(),
                        EnumTemplate::new(type_params.clone(), variants.clone()),
                    );
                }

                for (_, variant_types) in variants {
                    for variant_type in variant_types {
                        self.resolve_generic_type(variant_type);
                    }
                }

                self.find_unique_types(body);
            }

            UntypedNode::List(elements) => {
                for element in elements {
                    self.find_unique_types(element);
                }
            }

            UntypedNode::FunctionInstance(parent, sub_types) => {
                if let UntypedNode::Variable(enum_name) = &**parent {
                    // This isn't a function instance; it's an enum instance in disguise.
                    if self.enums.contains(enum_name) {
                        let enum_instance_type =
                            AstType::Generic(enum_name.clone(), sub_types.clone());

                        self.register_type(enum_name.clone(), enum_instance_type);
                    }
                };

                for sub_type in sub_types {
                    self.resolve_generic_type(sub_type);
                }
            }

            UntypedNode::CaseOf(expr, arms) => {
                self.find_unique_types(expr);

                for (pattern, value) in arms {
                    self.find_unique_types(pattern);
                    self.find_unique_types(value);
                }
            }

            UntypedNode::AnonymousRecord(..) => todo!("add generic checking to anonymous records"),
        }
    }

    fn expand_generic_declarations(&mut self, root: &UntypedNode) -> UntypedNode {
        match root {
            UntypedNode::Integer(..)
            | UntypedNode::Variable(..)
            | UntypedNode::Float(..)
            | UntypedNode::Str(..)
            | UntypedNode::Boolean(..) => root.to_owned(),

            UntypedNode::Get(field, parent) => UntypedNode::Get(
                field.clone(),
                Box::new(self.expand_generic_declarations(parent)),
            ),

            UntypedNode::RecordDeclaration(name, fields, type_params, body) => {
                // Don't need to check for Record Declarations, since at this point it is assumed
                // that all generic declarations have been expanded.
                UntypedNode::RecordDeclaration(
                    name.clone(),
                    fields
                        .iter()
                        .map(|(field_name, field_type)| {
                            (
                                field_name.clone(),
                                field_type.as_named_concrete(&self.enums),
                            )
                        })
                        .collect(),
                    Vec::new(),
                    Box::new(self.expand_generic_declarations(body)),
                )
            }

            UntypedNode::Group(body) => self.expand_generic_declarations(body),

            UntypedNode::Extern(extern_name, extern_type, body) => UntypedNode::Extern(
                extern_name.clone(),
                extern_type.clone(),
                Box::new(self.expand_generic_declarations(body)),
            ),

            UntypedNode::Let(name, var_type, var_value, body, is_recursive) => {
                let concrete_type = if let Some(generic_type @ AstType::Generic(..)) = var_type {
                    Some(AstType::Base(
                        generic_type.as_named_concrete(&self.enums).to_string(),
                    ))
                } else {
                    var_type.clone()
                };

                UntypedNode::Let(
                    name.clone(),
                    concrete_type,
                    Box::new(self.expand_generic_declarations(var_value)),
                    Box::new(self.expand_generic_declarations(body)),
                    *is_recursive,
                )
            }

            UntypedNode::Unary(value, operator) => UntypedNode::Unary(
                Box::new(self.expand_generic_declarations(value)),
                operator.clone(),
            ),

            UntypedNode::Binary(left_value, operator, right_value) => UntypedNode::Binary(
                Box::new(self.expand_generic_declarations(left_value)),
                operator.clone(),
                Box::new(self.expand_generic_declarations(right_value)),
            ),

            UntypedNode::Logical(left_value, operator, right_value) => UntypedNode::Logical(
                Box::new(self.expand_generic_declarations(left_value)),
                operator.clone(),
                Box::new(self.expand_generic_declarations(right_value)),
            ),

            UntypedNode::If(if_cond, then_expr, else_expr) => UntypedNode::If(
                Box::new(self.expand_generic_declarations(if_cond)),
                Box::new(self.expand_generic_declarations(then_expr)),
                Box::new(self.expand_generic_declarations(else_expr)),
            ),

            UntypedNode::Function(return_type, arguments, body) => UntypedNode::Function(
                return_type.as_named_concrete(&self.enums),
                arguments.clone(),
                Box::new(self.expand_generic_declarations(body)),
            ),

            UntypedNode::FunctionDeclaration(name, _, return_type, arguments, func_body, body) => {
                UntypedNode::FunctionDeclaration(
                    name.clone(),
                    Vec::new(),
                    return_type.as_named_concrete(&self.enums),
                    arguments
                        .iter()
                        .map(|(arg_name, arg_type)| {
                            (arg_name.clone(), arg_type.as_named_concrete(&self.enums))
                        })
                        .collect(),
                    Box::new(self.expand_generic_declarations(func_body)),
                    Box::new(self.expand_generic_declarations(body)),
                )
            }

            UntypedNode::FunctionCall(parent, arguments) => {
                // If generic, we can assume that the function's name must be a named function. Otherwise, keep expanding
                // as normal
                let new_name = if let UntypedNode::FunctionInstance(function, sub_types) = &**parent
                {
                    let original_name = match &**function {
                        UntypedNode::Variable(original_name) => original_name,
                        _ => panic!("expected generic function call to be named."),
                    };

                    let concrete_name = AstType::Generic(
                        original_name.clone(),
                        sub_types
                            .iter()
                            .map(|sub_type| sub_type.as_named_concrete(&self.enums))
                            .collect(),
                    )
                    .to_string();

                    UntypedNode::Variable(concrete_name)
                } else {
                    self.expand_generic_declarations(parent)
                };

                UntypedNode::FunctionCall(
                    Box::new(new_name),
                    arguments
                        .iter()
                        .map(|argument| self.expand_generic_declarations(argument))
                        .collect(),
                )
            }

            UntypedNode::RecordInstance(name, type_params, fields) => {
                let new_name = if !type_params.is_empty() {
                    AstType::Generic(
                        name.clone(),
                        type_params
                            .iter()
                            .map(|type_param| type_param.as_named_concrete(&self.enums))
                            .collect(),
                    )
                    .to_string()
                } else {
                    name.clone()
                };

                UntypedNode::RecordInstance(
                    new_name,
                    Vec::new(),
                    fields
                        .iter()
                        .map(|(field_name, field_value)| {
                            (
                                field_name.clone(),
                                self.expand_generic_declarations(field_value),
                            )
                        })
                        .collect(),
                )
            }

            UntypedNode::EnumDeclaration(name, variants, type_params, body) => {
                if !type_params.is_empty() {
                    // Expand the rest of the code first.
                    let expanded_body = self.expand_generic_declarations(body);

                    // Find the respective template, unique type parameters.
                    let template = self.state.get_template(name).unwrap();

                    // In case there are no instances of this generic functions, ignore it.
                    if let Some(types) = self.state.get_types(name) {
                        // Given these types, generate copies of the function template.
                        template.substitute(types, expanded_body)
                    } else {
                        expanded_body
                    }
                } else {
                    UntypedNode::EnumDeclaration(
                        name.clone(),
                        variants
                            .iter()
                            .map(|(variant_name, variant_types)| {
                                (
                                    variant_name.clone(),
                                    variant_types
                                        .iter()
                                        .map(|variant_type| {
                                            variant_type.as_named_concrete(&self.enums)
                                        })
                                        .collect(),
                                )
                            })
                            .collect(),
                        type_params.clone(),
                        Box::new(self.expand_generic_declarations(body)),
                    )
                }
            }

            UntypedNode::List(elements) => UntypedNode::List(
                elements
                    .iter()
                    .map(|element| self.expand_generic_declarations(element))
                    .collect(),
            ),

            UntypedNode::FunctionInstance(base, sub_types) => {
                // TODO: if this function instance is actually an enum, convert it to a concrete
                // type.
                let resolved_sub_types: Vec<AstType> = sub_types
                    .iter()
                    .map(|sub_type| sub_type.as_named_concrete(&self.enums))
                    .collect();

                let original_name = match &**base {
                    UntypedNode::Variable(original_name) => original_name,
                    _ => panic!("expected generic function call to be named."),
                };

                let concrete_name =
                    AstType::Generic(original_name.clone(), resolved_sub_types.clone()).to_string();

                UntypedNode::Variable(concrete_name)
            }

            UntypedNode::AnonymousRecord(..) => todo!("handle anonymous records"),

            UntypedNode::CaseOf(expr, arms) => UntypedNode::CaseOf(
                Box::new(self.expand_generic_declarations(expr)),
                arms.iter()
                    .map(|(pattern, value)| {
                        (
                            self.expand_generic_declarations(pattern),
                            self.expand_generic_declarations(value),
                        )
                    })
                    .collect(),
            ),
        }
    }
}
