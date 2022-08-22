struct BaseDimension(String);

#[derive(Debug, Clone, PartialEq)]
pub enum Dimension {
    Base(BaseDimension),
    Derived(String, Vec<(BaseDimension, i32)>),
}

impl Dimension {
    pub fn base(name: String) -> Self {
        Dimension::Base(BaseDimension(name))
    }

}
