/*
 * Generate common C constructs.
 * */

pub fn emit_unary(operator: String, expression: String) -> String {
    format!("{operator}{expression}")
}

pub fn emit_binary(left: String, operator: String, right: String) -> String {
    format!("{left}{operator}{right}")
}

pub fn emit_function_call(name: String, arguments: &[String], is_pointer: bool) -> String {
    if is_pointer {
        format!("(*{name})({})", arguments.join(", "))
    } else {
        format!("{name}({})", arguments.join(", "))
    }
}

pub fn emit_if(cond: String, then_expr: String, else_expr: String) -> String {
    format!("{cond} ? {then_expr} : {else_expr}")
}
