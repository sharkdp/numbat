use crate::type_variable::TypeVariable;

#[derive(Clone, Default)]
pub struct NameGenerator {
    counter: u64,
}

impl NameGenerator {
    pub fn fresh_type_variable(&mut self) -> TypeVariable {
        let name = format!("T{}", self.counter);
        self.counter += 1;
        TypeVariable::new(name)
    }
}
