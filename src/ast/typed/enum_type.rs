use std::rc::Rc;

use super::data_type::DataType;

pub fn get_variant_fields(enum_type: Rc<DataType>, variant: &String) -> Option<Vec<Rc<DataType>>> {
    match &*enum_type {
        DataType::Enum(_, variants) => variants.get(variant).map(|fields| fields.clone()),
        _ => None,
    }
}
