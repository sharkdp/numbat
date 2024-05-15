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
    "true",
    "false",
    "NaN",
    "inf",
    // procedures
    "print(",
    "assert(",
    "assert_eq(",
    "type(",
    // Type names
    "Bool",
    "String",
    "DateTime",
    // decorators
    "metric_prefixes",
    "binary_prefixes",
    "aliases",
    "name",
    "url",
];
