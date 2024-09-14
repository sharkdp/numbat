use crate::{prefix_parser::AcceptsPrefix, unit::CanonicalName};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decorator {
    MetricPrefixes,
    BinaryPrefixes,
    Aliases(Vec<(String, Option<AcceptsPrefix>)>),
    Url(String),
    Name(String),
    Description(String),
    Example(String, Option<String>),
}

pub fn name_and_aliases<'a>(
    name: &'a String,
    decorators: &'a [Decorator],
) -> Box<dyn Iterator<Item = (&'a String, AcceptsPrefix)> + 'a> {
    let aliases = {
        let mut aliases_vec = vec![];
        for decorator in decorators {
            if let Decorator::Aliases(aliases) = decorator {
                aliases_vec = aliases
                    .iter()
                    .map(|(name, accepts_prefix)| {
                        (name, accepts_prefix.unwrap_or(AcceptsPrefix::only_long()))
                    })
                    .collect();
            }
        }
        aliases_vec
    };

    if !aliases.iter().any(|(n, _)| n == &name) {
        let name_iter = std::iter::once((name, AcceptsPrefix::only_long()));
        Box::new(name_iter.chain(aliases))
    } else {
        Box::new(aliases.into_iter())
    }
}

pub fn get_canonical_unit_name(unit_name: &str, decorators: &[Decorator]) -> CanonicalName {
    for decorator in decorators {
        if let Decorator::Aliases(aliases) = decorator {
            for (alias, accepts_prefix) in aliases {
                match accepts_prefix {
                    &Some(ap) if ap.short => {
                        return CanonicalName::new(alias, ap);
                    }
                    _ => {}
                }
            }
        }
    }
    CanonicalName {
        name: unit_name.into(),
        accepts_prefix: AcceptsPrefix::only_long(),
    }
}

pub fn name(decorators: &[Decorator]) -> Option<String> {
    for decorator in decorators {
        if let Decorator::Name(name) = decorator {
            return Some(name.clone());
        }
    }
    None
}

pub fn url(decorators: &[Decorator]) -> Option<String> {
    for decorator in decorators {
        if let Decorator::Url(url) = decorator {
            return Some(url.clone());
        }
    }
    None
}

pub fn description(decorators: &[Decorator]) -> Option<String> {
    let mut description = String::new();
    for decorator in decorators {
        if let Decorator::Description(d) = decorator {
            description += d;
            description += "\n";
        }
    }
    if !description.is_empty() {
        Some(description)
    } else {
        None
    }
}

pub fn examples(decorators: &[Decorator]) -> Vec<(String, Option<String>)> {
    let mut examples = Vec::new();
    for decorator in decorators {
        if let Decorator::Example(example_code, example_description) = decorator {
            examples.push((example_code.clone(), example_description.clone()));
        }
    }
    return examples;
}

pub fn contains_aliases_with_prefixes(decorates: &[Decorator]) -> bool {
    for decorator in decorates {
        if let Decorator::Aliases(aliases) = decorator {
            if aliases.iter().any(|(_, prefixes)| prefixes.is_some()) {
                return true;
            }
        }
    }

    false
}

pub fn contains_aliases(decorators: &[Decorator]) -> bool {
    for decorator in decorators {
        if let Decorator::Aliases(_) = decorator {
            return true;
        }
    }

    false
}

pub fn contains_examples(decorators: &[Decorator]) -> bool {
    for decorator in decorators {
        if let Decorator::Example(_, _) = decorator {
            return true;
        }
    }

    false
}
