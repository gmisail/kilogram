use std::rc::Rc;

use crate::{compiler::resolver, ast::typed::data_type::DataType};

/// Generates a function which allocate memory from the stack onto the heap.
pub fn generate_box_constructor(var_type: Rc<DataType>) -> String {
    let resolved_type = resolver::get_native_type(var_type);
    let type_name = resolver::get_native_type_as_name(&resolved_type);
    let signature = format!("void* _kg_box_{type_name}({resolved_type} data)");

    let mut body = String::new(); 
    body.push_str(&format!("{resolved_type}* tmp = ({resolved_type}*) malloc(sizeof({resolved_type}));\n"));
    body.push_str(&format!("*tmp = data;\n"));
    body.push_str(&format!("return (void*) temp;"));

    format!("{signature}{{\n{body}\n}}\n")
}
