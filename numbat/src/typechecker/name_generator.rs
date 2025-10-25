use crate::type_variable::TypeVariable;

pub trait NameGenerator: std::fmt::Debug + Clone {
    fn fresh_type_variable(&mut self) -> TypeVariable;
}

#[derive(Clone, Default, Debug)]
pub struct DefaultNameGenerator {
    counter: u64,
}

impl NameGenerator for DefaultNameGenerator {
    fn fresh_type_variable(&mut self) -> TypeVariable {
        let name = format!("T{}", self.counter);
        self.counter += 1;
        TypeVariable::new(name)
    }
}
