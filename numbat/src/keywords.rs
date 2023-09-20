/// This is used for tab-completion and highlighting,
/// not for tokenizing/parsing.
pub const KEYWORDS: &[&str] = &[
    // keywords which are followed by a space
    "per ",
    "to ",
    "let ",
    "fn ",
    "dimension ",
    "unit ",
    "use ",
    // 'inline' keywords
    "long",
    "short",
    "both",
    "none",
    "if",
    "then",
    "else",
    "bool",
    "true",
    "false",
    "str",
    // decorators
    "metric_prefixes",
    "binary_prefixes",
    "aliases",
    // procedures
    "print(",
    "assert_eq(",
    "type(",
];
