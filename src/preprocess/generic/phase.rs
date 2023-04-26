use std::collections::{HashMap, HashSet};

use crate::ast::untyped::ast_type::AstType;
use crate::ast::untyped::untyped_node::UntypedNode;

use crate::preprocess::generic::template::RecordTemplate;
use crate::preprocess::PreprocessPhase;

pub struct GenericPhase {
    types: HashMap<String, HashSet<AstType>>,
    templates: HashMap<String, RecordTemplate>,
}

impl GenericPhase {
    pub fn new() -> Self {
        GenericPhase {
            types: HashMap::new(),
            templates: HashMap::new(),
        }
    }
}

impl GenericPhase {
    fn resolve_generic_type(&mut self, t: &AstType) {
        todo!("resolve generic type as a concrete type")
    }

    /// Search the AST for generic types and save all unique configurations.
    ///  i.e. Pair(int, int), Pair(float, int), etc...
    ///
    /// * `root`: node to search
    fn find_unique_types(&mut self, root: &UntypedNode) {
        match root {
            UntypedNode::Integer(..)
            | UntypedNode::Variable(..)
            | UntypedNode::Float(..)
            | UntypedNode::Str(..)
            | UntypedNode::Boolean(..)
            | UntypedNode::Get(..) => {
                return;
            }

            UntypedNode::Group(body) | UntypedNode::Extern(_, _, body) => {
                self.find_unique_types(body);
            }

            UntypedNode::RecordDeclaration(name, fields, type_params, body) => {
                // If we have more than one type parameter, then this is a generic record.
                if !type_params.is_empty() {
                    self.templates.insert(
                        name.clone(),
                        RecordTemplate::new(name.clone(), type_params.clone(), fields.clone()),
                    );
                }

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

            UntypedNode::Function(_, return_type, arguments, body) => {
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

            UntypedNode::RecordInstance(name, type_params, fields) => {
                // Resolve every type parameter, in case they contain a generic type.
                for type_param in type_params {
                    self.resolve_generic_type(type_param);
                }

                // If the type is generic, add it to the list of generic type configurations.
                if type_params.len() > 0 {
                    self.types
                        .entry(name.clone())
                        .or_insert(HashSet::new())
                        .insert(AstType::Generic(name.clone(), type_params.clone()));
                }

                // Recurse through each of the field values and search for generic types.
                for (_, field_value) in fields {
                    self.find_unique_types(field_value);
                }
            }

            UntypedNode::EnumDeclaration(_name, _variants, _type_params, _body) => {
                todo!("add generic enum checking")
            }

            UntypedNode::List(elements) => {
                for element in elements {
                    self.find_unique_types(element);
                }
            }

            UntypedNode::AnonymousRecord(..) => todo!("add generic checking to anonymous records"),
            UntypedNode::CaseOf(_expr, _arms) => todo!("add generic checking to case of"),
        }
    }

    /// Given a root, expand the generic declarations into monomorphized versions.
    ///     i.e. Pair('T, 'S) with (int, int) & (float, string) => Pair_int_int, Pair_float_string
    ///
    /// * `root`: node to start expansion
    fn expand_generic_declarations(&mut self, root: &UntypedNode) -> UntypedNode {
        match root {
            UntypedNode::Integer(..)
            | UntypedNode::Variable(..)
            | UntypedNode::Float(..)
            | UntypedNode::Str(..)
            | UntypedNode::Boolean(..)
            | UntypedNode::Get(..) => root.to_owned(),

            UntypedNode::RecordDeclaration(name, fields, type_params, body) => {
                // If we have more than one type parameter, then this is a generic record.
                //
                // TODO: test this
                if !type_params.is_empty() {
                    self.templates.insert(
                        name.clone(),
                        RecordTemplate::new(name.clone(), type_params.clone(), fields.clone()),
                    );

                    self.transform(body)
                } else {
                    root.to_owned()
                }
            }

            UntypedNode::Group(body) => self.expand_generic_declarations(body),

            UntypedNode::Extern(extern_name, extern_type, body) => UntypedNode::Extern(
                extern_name.clone(),
                extern_type.clone(),
                Box::new(self.expand_generic_declarations(body)),
            ),

            UntypedNode::Let(name, var_type, var_value, body, is_recursive) => UntypedNode::Let(
                name.clone(),
                var_type.clone(),
                Box::new(self.expand_generic_declarations(var_value)),
                Box::new(self.expand_generic_declarations(body)),
                *is_recursive,
            ),

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

            UntypedNode::Function(func_type, return_type, arguments, body) => {
                UntypedNode::Function(
                    func_type.clone(),
                    return_type.clone(),
                    arguments.clone(),
                    Box::new(self.expand_generic_declarations(body)),
                )
            }

            UntypedNode::FunctionCall(parent, arguments) => UntypedNode::FunctionCall(
                Box::new(self.expand_generic_declarations(parent)),
                arguments
                    .iter()
                    .map(|argument| self.expand_generic_declarations(argument))
                    .collect(),
            ),

            UntypedNode::RecordInstance(name, type_params, fields) => {
                // TODO: create instance of template if generic

                /*
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
                )*/

                todo!()
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
                    Box::new(self.expand_generic_declarations(body)),
                )
            }

            UntypedNode::List(elements) => UntypedNode::List(
                elements
                    .iter()
                    .map(|element| self.expand_generic_declarations(element))
                    .collect(),
            ),

            UntypedNode::AnonymousRecord(..) => todo!(),

            UntypedNode::CaseOf(_expr, _arms) => {
                todo!("add generic checking to case of")
            }
        }
    }
}

impl PreprocessPhase for GenericPhase {
    fn transform(&mut self, root: &UntypedNode) -> UntypedNode {
        // First, search the AST for usages of generic types.
        self.find_unique_types(root);

        // Once these are found, convert them into concrete type declarations.
        self.expand_generic_declarations(root)
    }
}
