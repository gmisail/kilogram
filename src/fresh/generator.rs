/// Generates unique variables for a given namespace, i.e. "variable" or "lambda"
pub struct FreshGenerator {
    subject: &'static str,
    index: usize,
}

impl FreshGenerator {
    /// Create a new FreshGenerator with a subject.
    ///
    /// * `subject`: the fresh variable namespace, i.e. "variable", "lambda"
    pub fn new(subject: &'static str) -> Self {
        FreshGenerator { subject, index: 0 }
    }
}

impl Iterator for FreshGenerator {
    type Item = String;

    /// Generates a new fresh variable name.
    fn next(&mut self) -> Option<Self::Item> {
        self.index += 1;

        Some(format!("_kg_{}_{}", self.subject, self.index - 1))
    }
}

#[cfg(test)]
mod tests {
    use crate::fresh::generator::FreshGenerator;

    #[test]
    fn generates_fresh_variable() {
        let mut test_gen = FreshGenerator::new("test");

        assert_eq!("_kg_test_0", test_gen.next().unwrap());
        assert_eq!("_kg_test_1", test_gen.next().unwrap());
        assert_eq!("_kg_test_2", test_gen.next().unwrap());

        let mut var_gen = FreshGenerator::new("var");

        assert_eq!("_kg_var_0", var_gen.next().unwrap());
        assert_eq!("_kg_var_1", var_gen.next().unwrap());
        assert_eq!("_kg_var_2", var_gen.next().unwrap());
    }
}
