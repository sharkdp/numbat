"""
Pygments lexer for the Numbat programming language.

This lexer is used by Zensical/MkDocs for syntax highlighting in documentation.
"""

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import (
    Comment,
    Keyword,
    Name,
    Number,
    Operator,
    Punctuation,
    String,
    Text,
    Whitespace,
)


class NumbatLexer(RegexLexer):
    """Pygments lexer for the Numbat programming language."""

    name = "Numbat"
    aliases = ["numbat", "nbt"]
    filenames = ["*.nbt"]
    mimetypes = ["text/x-numbat"]

    tokens = {
        "root": [
            # Whitespace
            (r"\s+", Whitespace),
            # Comments
            (r"#.*$", Comment.Single),
            # REPL prompts (common in documentation)
            (r"^>>>.*$", Comment.Special),
            # Strings
            (r'"[^"]*"', String.Double),
            # Decorators/Attributes
            (
                r"@(aliases|metric_prefixes|binary_prefixes|name|url|description)\b",
                Name.Decorator,
            ),
            # Keywords
            (
                words(
                    (
                        "per",
                        "to",
                        "let",
                        "fn",
                        "where",
                        "and",
                        "dimension",
                        "unit",
                        "use",
                        "struct",
                        "long",
                        "short",
                        "both",
                        "none",
                        "if",
                        "then",
                        "else",
                        "true",
                        "false",
                        "print",
                        "assert",
                        "assert_eq",
                        "type",
                    ),
                    prefix=r"\b",
                    suffix=r"\b",
                ),
                Keyword,
            ),
            # Special numeric values
            (r"\b(NaN|inf)\b", Number.Float),
            # Hexadecimal numbers
            (r"\b0x[0-9a-fA-F]+\b", Number.Hex),
            # Octal numbers
            (r"\b0o[0-7]+\b", Number.Oct),
            # Binary numbers
            (r"\b0b[01]+\b", Number.Bin),
            # Floating point numbers (with optional exponent)
            (r"\b[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9]+)?\b", Number.Float),
            (r"\b[0-9][0-9_]*[eE][+-]?[0-9]+\b", Number.Float),
            # Integers
            (r"\b[0-9][0-9_]*\b", Number.Integer),
            # Types/Dimensions (capitalized identifiers)
            (r"\b[A-Z][a-zA-Z0-9_]*\b", Name.Class),
            # Arrow operators
            (r"->|→|➞", Operator),
            # Comparison operators
            (r"==|!=|<=|>=|<|>", Operator),
            # Mathematical operators
            (r"[+\-*/^÷×·⋅²³]", Operator),
            # Assignment and type annotation
            (r"[:=]", Operator),
            # Punctuation
            (r"[(),\[\]{}]", Punctuation),
            # Unicode degree symbol
            (r"°", Name.Other),
            # Greek letters and other Unicode (common in scientific notation)
            (r"[α-ωΑ-Ωπℏ€£¥₹]", Name.Other),
            # Identifiers (lowercase start)
            (r"\b[a-z_][a-zA-Z0-9_]*\b", Name.Variable),
        ],
    }
