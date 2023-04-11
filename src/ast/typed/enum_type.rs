use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use super::data_type::DataType;

/// Get variants from a type, if it has them.
pub fn get_variants(enum_type: Rc<DataType>) -> Option<BTreeMap<String, Vec<Rc<DataType>>>> {
    match &*enum_type {
        DataType::Enum(_, variants) => Some(variants.clone()),
        _ => None,
    }
}

/// Get all fields for a specific variant.
pub fn get_variant_fields(enum_type: Rc<DataType>, variant: &String) -> Option<Vec<Rc<DataType>>> {
    match &*enum_type {
        DataType::Enum(_, variants) => variants.get(variant).cloned(),
        _ => None,
    }
}

/// Creates a mapping between variants and their parent types.
///     e.g. [Cons -> List, Some -> Option]
pub fn create_variant_parent_map(
    enums: &HashMap<String, Rc<DataType>>,
) -> HashMap<String, Rc<DataType>> {
    let mut all_variants: HashMap<String, Rc<DataType>> = HashMap::new();
    for enum_type in enums.values() {
        let variant_bindings: HashMap<String, Rc<DataType>> = get_variants(enum_type.clone())
            .expect("enum {name} to have fields")
            .keys()
            .map(|variant_name| (variant_name.clone(), enum_type.clone()))
            .collect();

        all_variants.extend(variant_bindings);
    }

    all_variants
}
