/*
 * Generates unique function names.
 * */

pub struct FunctionGenerator {
    index: usize,
}

impl FunctionGenerator {
    pub fn new() -> Self {
        FunctionGenerator { index: 0 }
    }

    pub fn generate(&mut self) -> String {
        self.index += 1;

        format!("_lambda_{}", self.index)
    }
}
