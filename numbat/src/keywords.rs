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
    "struct ",
    // 'inline' keywords
    "long",
    "short",
    "both",
    "none",
    "if",
    "then",
    "else",
    "Bool",
    "true",
    "false",
    "String",
    "DateTime",
    "NaN",
    "inf",
    // decorators
    "metric_prefixes",
    "binary_prefixes",
    "aliases",
    "name",
    "url",
    // procedures
    "print(",
    "assert_eq(",
    "assert(",
    "type(",
];
