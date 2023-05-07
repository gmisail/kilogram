use std::collections::{BTreeSet, HashMap};

use crate::ast::untyped::ast_type::AstType;
use crate::ast::untyped::untyped_node::UntypedNode;

use crate::preprocess::generic::template::Template;
use crate::preprocess::PreprocessPhase;

use super::record_template::RecordTemplate;

pub struct GenericPhase {
    // Ensures uniqueness without needing to check every t
    all_types: BTreeSet<AstType>,
    types: HashMap<String, Vec<AstType>>,
    templates: HashMap<String, RecordTemplate>,
    // TODO: add function templates
}

impl GenericPhase {
    pub fn new() -> Self {
        GenericPhase {
            all_types: BTreeSet::new(),
            types: HashMap::new(),
            templates: HashMap::new(),
        }
    }
}

impl GenericPhase {
    /// Recurse through a type searching for unique generic type instances, creating them
    /// if they do not exist.
    ///
    /// * `ast_type`: type to search on
    fn resolve_generic_type(&mut self, ast_type: &AstType) {
        match ast_type {
            // Base types (int, float, etc...) can't be generic.
            AstType::Base(_) => (),

            AstType::Generic(name, sub_types) => {
                for sub_type in sub_types {
                    self.resolve_generic_type(sub_type);
                }

                let new_type = AstType::Generic(name.clone(), sub_types.clone());

                self.register_type(name.clone(), new_type);
            }

            AstType::Function(arguments, return_type) => {
                for argument in arguments {
                    self.resolve_generic_type(argument);
                }

                self.resolve_generic_type(return_type);
            }
            AstType::Record(fields) => {
                for (_, field_type) in fields {
                    self.resolve_generic_type(field_type);
                }
            }
        }
    }

    fn register_type(&mut self, name: String, ast_type: AstType) {
        if !self.all_types.contains(&ast_type) {
            self.all_types.insert(ast_type.clone());

            self.types.entry(name).or_insert(Vec::new()).push(ast_type);
        }
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
                        RecordTemplate::new(type_params.clone(), fields.clone()),
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

            UntypedNode::FunctionDeclaration(_, _, return_type, arguments, func_body, body) => {
                // Find generic types in both the return type and arguments.
                self.resolve_generic_type(return_type);

                for (_, arg_type) in arguments {
                    self.resolve_generic_type(arg_type);
                }

                // TODO: how do we handle generic instances within the function body?
                //       for instance, if we have:
                //
                //       function make_pair['T, 'S](...): Pair['T, 'S]
                //          Pair['T, 'S] { ... }
                //       end
                //
                //       How do we know what types to register for Pair? Separate generic
                //       resolution phases for records, types, and enums maybe?

                self.find_unique_types(func_body);
                self.find_unique_types(body);
            }

            UntypedNode::RecordInstance(name, type_params, fields) => {
                // Resolve every type parameter, in case they contain a generic type.
                for type_param in type_params {
                    self.resolve_generic_type(type_param);
                }

                // If the type is generic, add it to the list of generic type configurations.
                if !type_params.is_empty() {
                    self.register_type(
                        name.clone(),
                        AstType::Generic(name.clone(), type_params.clone()),
                    );
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
                // More than one type parameter? Must be generic record.
                if !type_params.is_empty() {
                    // Expand the rest of the AST first
                    let expanded_body = self.expand_generic_declarations(body);

                    let template = self.templates.get(name).unwrap();
                    let types = self.types.get(name).unwrap().clone();

                    template.substitute(&types, expanded_body)
                } else {
                    UntypedNode::RecordDeclaration(
                        name.clone(),
                        fields
                            .iter()
                            .map(|(field_name, field_type)| {
                                (field_name.clone(), field_type.convert_generic_to_concrete())
                            })
                            .collect(),
                        Vec::new(),
                        Box::new(self.expand_generic_declarations(body)),
                    )
                }
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
                        generic_type.convert_generic_to_concrete().to_string(),
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
                return_type.clone(),
                arguments.clone(),
                Box::new(self.expand_generic_declarations(body)),
            ),

            UntypedNode::FunctionDeclaration(
                name,
                type_params,
                return_type,
                arguments,
                func_body,
                body,
            ) => {
                if !type_params.is_empty() {
                    todo!("make monomorphized copies for {name}...")
                } else {
                    // Not generic? Don't apply any substitutions, just convert types to concrete
                    // and recurse.
                    UntypedNode::FunctionDeclaration(
                        name.clone(),
                        Vec::new(),
                        return_type.convert_generic_to_concrete(),
                        arguments
                            .iter()
                            .map(|(arg_name, arg_type)| {
                                (arg_name.clone(), arg_type.convert_generic_to_concrete())
                            })
                            .collect(),
                        Box::new(self.expand_generic_declarations(func_body)),
                        Box::new(self.expand_generic_declarations(body)),
                    )
                }
            }

            UntypedNode::FunctionCall(parent, arguments) => UntypedNode::FunctionCall(
                Box::new(self.expand_generic_declarations(parent)),
                arguments
                    .iter()
                    .map(|argument| self.expand_generic_declarations(argument))
                    .collect(),
            ),

            UntypedNode::RecordInstance(name, type_params, fields) => {
                let new_name = if !type_params.is_empty() {
                    AstType::Generic(
                        name.clone(),
                        type_params
                            .iter()
                            .map(|type_param| type_param.convert_generic_to_concrete())
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
        let res = self.expand_generic_declarations(root);

        println!("{res:#?}");

        res
    }
}
