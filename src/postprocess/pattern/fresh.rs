struct FreshGenerator {
    count: usize,
}

impl FreshGenerator {
    fn new() -> Self {
        FreshGenerator { count: 0 }
    }

    fn next(&mut self) -> String {
        self.count += 1;

        format!("fresh_{}", self.count)
    }
}
