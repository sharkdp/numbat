#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decorator {
    MetricPrefixes,
    BinaryPrefixes,
    Aliases(Vec<String>),
    AliasesShort(Vec<String>),
}

pub fn name_and_aliases<'a>(
    name: &'a String,
    decorators: &'a [Decorator],
) -> Box<dyn Iterator<Item = &'a String> + 'a> {
    let name_iter = std::iter::once(name);
    for decorator in decorators {
        if let Decorator::Aliases(aliases) = decorator {
            return Box::new(name_iter.chain(aliases.iter()));
        }
    }
    Box::new(name_iter)
}

pub fn aliases_short<'a>(decorators: &'a [Decorator]) -> Box<dyn Iterator<Item = &'a String> + 'a> {
    for decorator in decorators {
        if let Decorator::AliasesShort(aliases) = decorator {
            return Box::new(aliases.iter());
        }
    }
    Box::new(std::iter::empty())
}
