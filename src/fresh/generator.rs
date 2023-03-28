use std::cell::Cell;

thread_local! {
    pub static INDEX: Cell<usize> = Cell::new(0);
}

pub fn fresh_variable(subject: &'static str) -> String {
    let index = INDEX.with(|i| {
        let res = i.get();
        i.set(res + 1);
        res
    });

    format!("_kg_{subject}_{index}")
}

#[cfg(test)]
mod tests {
    use crate::fresh::generator::fresh_variable;

    #[test]
    fn generates_fresh_variable() {
        assert_eq!("_kg_test_0", fresh_variable("test"));
        assert_eq!("_kg_test_1", fresh_variable("test"));
        assert_eq!("_kg_test_2", fresh_variable("test"));

        assert_eq!("_kg_var_3", fresh_variable("var"));
        assert_eq!("_kg_var_4", fresh_variable("var"));
        assert_eq!("_kg_var_5", fresh_variable("var"));
    }
}
