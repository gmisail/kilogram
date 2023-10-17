use std::collections::{BTreeSet, HashMap};
use tracing::info;

use crate::ast::untyped::ast_type::AstType;
use crate::ast::untyped::untyped_node::UntypedNode;
use crate::ast::untyped::untyped_node::UntypedNode::*;
use crate::preprocess::generic::function_pass::template::FunctionTemplate;

use crate::preprocess::generic::template::Template;

///
///     GENERIC - FUNCTION PASS
///
///     Walks through the AST looking for
///         a) generic type declarations
///         b) unique invocations of generic types
///
///     Given these unique invocations, walk through again and replace the generic calls
///     with calls to concrete types.
///

// TODO: add type parameter collision detection, i.e. if there is 'T defined within a generic
// type that defines 'T, throw an error.

pub struct FunctionPass {
    // Ensures uniqueness without needing to check every type
    all_types: BTreeSet<AstType>,
    types: HashMap<String, Vec<AstType>>,
    templates: HashMap<String, FunctionTemplate>,
}

impl FunctionPass {
    pub fn new() -> Self {
        FunctionPass {
            all_types: BTreeSet::new(),
            types: HashMap::new(),
            templates: HashMap::new(),
        }
    }

    pub fn apply(&mut self, node: &UntypedNode) -> UntypedNode {
        // First, search the AST for usages of generic types.
        self.find_unique_types(node);

        // Once these are found, convert them into concrete type declarations.
        self.expand_generic_declarations(node)
    }

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
            Function(return_type, arguments, body) => {
                // Find generic types in both the return type and arguments.
                self.resolve_generic_type(return_type);

                for (_, arg_type) in arguments {
                    self.resolve_generic_type(arg_type);
                }

                self.find_unique_types(body);
            }

            FunctionCall(parent, arguments) => {
                self.find_unique_types(parent);

                if let FunctionInstance(base, sub_types) = &**parent {
                    for sub_type in sub_types {
                        self.resolve_generic_type(sub_type);
                    }

                    if let Variable(function_name) = &**base {
                        // TODO: maybe we should separate this into separate sets, i.e. not mix functions and other types?
                        self.register_type(
                            function_name.clone(),
                            AstType::Generic(function_name.clone(), sub_types.clone()),
                        );
                    } else {
                        panic!("Generic functions must be named.")
                    }
                }

                for argument in arguments {
                    self.find_unique_types(argument);
                }
            }

            FunctionDeclaration(name, type_params, return_type, func_params, func_body, body) => {
                // Find generic types in both the return type and arguments.
                self.resolve_generic_type(return_type);

                for (_, param_type) in func_params {
                    self.resolve_generic_type(param_type);
                }

                if !type_params.is_empty() {
                    let inserted_template = self.templates.insert(
                        name.clone(),
                        FunctionTemplate::new(
                            type_params.clone(),
                            func_params.clone(),
                            return_type.clone(),
                            (**func_body).clone(),
                        ),
                    );

                    if inserted_template.is_some() {
                        panic!(
                            "TODO: handle this gracefully. This function template already exists."
                        );
                    }
                }

                self.find_unique_types(func_body);
                self.find_unique_types(body);
            }

            FunctionInstance(parent, sub_types) => {
                let name = match &**parent {
                    Variable(function_name) => function_name,
                    _ => panic!("expected generic function call to be named."),
                };

                for sub_type in sub_types {
                    self.resolve_generic_type(sub_type);
                }

                self.register_type(
                    name.clone(),
                    AstType::Generic(name.clone(), sub_types.clone()),
                );
            }

            Integer(..) | Variable(..) | Float(..) | Str(..) | Boolean(..) => {}

            Get(_, parent) => {
                self.find_unique_types(parent);
            }

            Group(body) | Extern(_, _, body) => {
                self.find_unique_types(body);
            }

            Let(_, var_type, var_value, body, _) => {
                // If type is explicit, check if it contains a generic type.
                if let Some(t) = var_type {
                    self.resolve_generic_type(t);
                }

                self.find_unique_types(var_value);
                self.find_unique_types(body);
            }

            Unary(value, _) => {
                self.find_unique_types(value);
            }

            Binary(left_value, _, right_value) | Logical(left_value, _, right_value) => {
                self.find_unique_types(left_value);
                self.find_unique_types(right_value);
            }

            If(if_cond, then_expr, else_expr) => {
                self.find_unique_types(if_cond);
                self.find_unique_types(then_expr);
                self.find_unique_types(else_expr);
            }

            RecordInstance(_, _, fields) => {
                for (_, field_value) in fields {
                    self.find_unique_types(field_value);
                }
            }

            RecordDeclaration(_, _, _, body) => self.find_unique_types(body),

            EnumDeclaration(_name, _variants, _type_params, _body) => {
                todo!("add generic enum checking")
            }

            List(elements) => {
                for element in elements {
                    self.find_unique_types(element);
                }
            }

            AnonymousRecord(..) => todo!("add generic checking to anonymous records"),
            CaseOf(_expr, _arms) => todo!("add generic checking to case of"),
        }
    }

    /// Given a root, expand the generic declarations into monomorphized versions.
    ///     i.e. Pair('T, 'S) with (int, int) & (float, string) => Pair_int_int, Pair_float_string
    ///
    /// * `root`: node to start expansion
    fn expand_generic_declarations(&mut self, root: &UntypedNode) -> UntypedNode {
        match root {
            Integer(..) | Variable(..) | Float(..) | Str(..) | Boolean(..) => root.to_owned(),

            Get(_, parent) => self.expand_generic_declarations(parent),

            RecordDeclaration(name, fields, type_params, body) => RecordDeclaration(
                name.clone(),
                fields.clone(),
                type_params.clone(),
                Box::new(self.expand_generic_declarations(body)),
            ),

            Group(body) => self.expand_generic_declarations(body),

            Extern(extern_name, extern_type, body) => Extern(
                extern_name.clone(),
                extern_type.clone(),
                Box::new(self.expand_generic_declarations(body)),
            ),

            Let(name, var_type, var_value, body, is_recursive) => {
                // TODO: do we need this, we're not going to be converting generic records to concrete types in this phase.
                let concrete_type = if let Some(generic_type @ AstType::Generic(..)) = var_type {
                    Some(AstType::Base(
                        generic_type.convert_generic_to_concrete().to_string(),
                    ))
                } else {
                    var_type.clone()
                };

                Let(
                    name.clone(),
                    concrete_type,
                    Box::new(self.expand_generic_declarations(var_value)),
                    Box::new(self.expand_generic_declarations(body)),
                    *is_recursive,
                )
            }

            Unary(value, operator) => Unary(
                Box::new(self.expand_generic_declarations(value)),
                operator.clone(),
            ),

            Binary(left_value, operator, right_value) => Binary(
                Box::new(self.expand_generic_declarations(left_value)),
                operator.clone(),
                Box::new(self.expand_generic_declarations(right_value)),
            ),

            Logical(left_value, operator, right_value) => Logical(
                Box::new(self.expand_generic_declarations(left_value)),
                operator.clone(),
                Box::new(self.expand_generic_declarations(right_value)),
            ),

            If(if_cond, then_expr, else_expr) => If(
                Box::new(self.expand_generic_declarations(if_cond)),
                Box::new(self.expand_generic_declarations(then_expr)),
                Box::new(self.expand_generic_declarations(else_expr)),
            ),

            Function(return_type, arguments, body) => Function(
                return_type.clone(),
                arguments.clone(),
                Box::new(self.expand_generic_declarations(body)),
            ),

            FunctionDeclaration(name, type_params, return_type, arguments, func_body, body) => {
                if !type_params.is_empty() {
                    // Expand the rest of the code first.
                    let expanded_body = self.expand_generic_declarations(body);

                    // Find the respective template, unique type parameters.
                    let template = self.templates.get(name).unwrap();

                    // In case there are no instances of this generic functions, ignore it.
                    if let Some(types) = self.types.get(name) {
                        // Given these types, generate copies of the function template.
                        template.substitute(&types, expanded_body)
                    } else {
                        expanded_body
                    }
                } else {
                    /*
                        Not generic? Don't apply any substitutions, just convert types to concrete
                        and recurse.
                    */
                    FunctionDeclaration(
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

            FunctionCall(parent, arguments) => {
                // If generic, we can assume that the function's name must be a named function.
                // Otherwise, keep expanding as normal
                let new_name = if let FunctionInstance(function, sub_types) = &**parent {
                    let original_name = match &**function {
                        Variable(original_name) => original_name,
                        _ => panic!("expected generic function call to be named."),
                    };

                    let concrete_name = AstType::Generic(
                        original_name.clone(),
                        sub_types
                            .iter()
                            .map(|sub_type| sub_type.convert_generic_to_concrete())
                            .collect(),
                    )
                    .to_string();

                    Variable(concrete_name)
                } else {
                    self.expand_generic_declarations(parent)
                };

                FunctionCall(
                    Box::new(new_name),
                    arguments
                        .iter()
                        .map(|argument| self.expand_generic_declarations(argument))
                        .collect(),
                )
            }

            RecordInstance(name, type_params, fields) => RecordInstance(
                name.clone(),
                type_params.clone(),
                fields
                    .iter()
                    .map(|(field_name, field_value)| {
                        (
                            field_name.clone(),
                            self.expand_generic_declarations(field_value),
                        )
                    })
                    .collect(),
            ),

            EnumDeclaration(name, variants, type_params, body) => {
                EnumDeclaration(
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

            List(elements) => List(
                elements
                    .iter()
                    .map(|element| self.expand_generic_declarations(element))
                    .collect(),
            ),

            AnonymousRecord(..) => todo!("handle anonymous records"),
            FunctionInstance(..) => todo!("handle generic functions"),

            CaseOf(_expr, _arms) => {
                todo!("add generic checking to case of")
            }
        }
    }
}
