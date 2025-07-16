/// This is used for tab-completion and highlighting,
/// not for tokenizing/parsing.
pub const KEYWORDS: &[&str] = &[
    // keywords which are followed by a space
    "per ",
    "to ",
    "let ",
    "fn ",
    "where ",
    "dimension ",
    "unit ",
    "use ",
    "struct ",
    "xor ",
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
    "Fn",
    "List",
    // decorators
    "metric_prefixes",
    "binary_prefixes",
    "aliases",
    "name",
    "url",
];
