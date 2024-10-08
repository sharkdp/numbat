use crate::{prefix_parser::AcceptsPrefix, span::Span, unit::CanonicalName};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decorator<'a> {
    MetricPrefixes,
    BinaryPrefixes,
    Aliases(Vec<(&'a str, Option<AcceptsPrefix>, Span)>),
    Url(String),
    Name(String),
    Description(String),
    Example(String, Option<String>),
}

/// Get an iterator of data computed from a name and/or its alias's `AcceptsPrefix` and
/// `Span`. If `name` itself is in the list of aliases, then it (or more precisely, the
/// data computed from it) will be placed at the front of the iterator
///
/// `f` says how to turn a triple of data associated with `name` or an alias, `(name,
/// accepts_prefix, Option<span>)`, into a `T`. The generality is really just here to
/// decide whether to yield `(&'a String, AcceptsPrefix)` or a `(&'a String,
/// AcceptsPrefix, Span)`.
fn name_and_aliases_inner<'a, T: 'a>(
    name: &'a str,
    decorators: &'a [Decorator],
    f: impl 'a + Fn(&'a str, AcceptsPrefix, Option<Span>) -> T,
) -> impl 'a + Iterator<Item = T> {
    // contains all the aliases of `name`, starting with `name` itself
    let mut aliases_vec = vec![f(name, AcceptsPrefix::only_long(), None)];

    for decorator in decorators {
        if let Decorator::Aliases(aliases) = decorator {
            for (n, ap, span) in aliases {
                let ap = ap.unwrap_or(AcceptsPrefix::only_long());
                if *n == name {
                    // use the AcceptsPrefix from the alias, but the span from `name`
                    // itself; this way we always report a conflicting `name` first
                    // before reporting any of its aliases. in effect we swallow aliases
                    // equal to `name` itself (but keep their metadata)
                    aliases_vec[0] = f(n, ap, None);
                } else {
                    aliases_vec.push(f(n, ap, Some(*span)));
                }
            }
        }
    }

    aliases_vec.into_iter()
}

/// Returns iterator of `(name_or_alias, accepts_prefix)` for the given name
pub fn name_and_aliases<'a>(
    name: &'a str,
    decorators: &'a [Decorator],
) -> impl 'a + Iterator<Item = (&'a str, AcceptsPrefix)> {
    name_and_aliases_inner(name, decorators, |n, accepts_prefix, _| (n, accepts_prefix))
}

/// Returns iterator of `(name_or_alias, accepts_prefix, span)` for the given name
pub fn name_and_aliases_spans<'a>(
    name: &'a str,
    name_span: Span,
    decorators: &'a [Decorator],
) -> impl 'a + Iterator<Item = (&'a str, AcceptsPrefix, Span)> {
    name_and_aliases_inner(name, decorators, move |n, accepts_prefix, span| {
        (n, accepts_prefix, span.unwrap_or(name_span))
    })
}

pub fn get_canonical_unit_name(unit_name: &str, decorators: &[Decorator]) -> CanonicalName {
    for decorator in decorators {
        if let Decorator::Aliases(aliases) = decorator {
            for (alias, accepts_prefix, _) in aliases {
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

pub fn name<'a>(decorators: &'a [Decorator<'a>]) -> Option<&'a str> {
    for decorator in decorators {
        if let Decorator::Name(name) = decorator {
            return Some(name);
        }
    }
    None
}

pub fn url<'a>(decorators: &'a [Decorator<'a>]) -> Option<&'a str> {
    for decorator in decorators {
        if let Decorator::Url(url) = decorator {
            return Some(url);
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
    examples
}

pub fn contains_aliases_with_prefixes(decorates: &[Decorator]) -> bool {
    for decorator in decorates {
        if let Decorator::Aliases(aliases) = decorator {
            if aliases.iter().any(|(_, prefixes, _)| prefixes.is_some()) {
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
