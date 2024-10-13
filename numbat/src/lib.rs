mod arithmetic;
mod ast;
#[cfg(feature = "html-formatter")]
pub mod buffered_writer;
mod bytecode_interpreter;
mod column_formatter;
pub mod command;
mod currency;
mod datetime;
mod decorator;
pub mod diagnostic;
mod dimension;
mod ffi;
mod gamma;
pub mod help;
#[cfg(feature = "html-formatter")]
pub mod html_formatter;
mod interpreter;
pub mod keywords;
pub mod list;
pub mod markup;
mod math;
pub mod module_importer;
mod name_resolution;
mod number;
mod parser;
mod plot;
mod prefix;
mod prefix_parser;
mod prefix_transformer;
pub mod pretty_print;
mod product;
mod quantity;
mod registry;
pub mod resolver;
pub mod session_history;
mod span;
mod suggestion;
mod tokenizer;
mod traversal;
mod type_variable;
mod typechecker;
mod typed_ast;
pub mod unicode_input;
mod unit;
mod unit_registry;
pub mod value;
mod vm;

use std::borrow::Cow;

use bytecode_interpreter::BytecodeInterpreter;
use column_formatter::ColumnFormatter;
use compact_str::CompactString;
use compact_str::CompactStringExt;
use compact_str::ToCompactString;
use currency::ExchangeRatesCache;
use diagnostic::ErrorDiagnostic;
use dimension::DimensionRegistry;
use interpreter::Interpreter;
use keywords::KEYWORDS;
use markup as m;
use markup::FormatType;
use markup::Markup;
use module_importer::{ModuleImporter, NullImporter};
use prefix_transformer::Transformer;

use resolver::CodeSource;
use resolver::Resolver;
use resolver::ResolverError;
use thiserror::Error;
use typechecker::{TypeCheckError, TypeChecker};

pub use diagnostic::Diagnostic;
pub use interpreter::InterpreterResult;
pub use interpreter::InterpreterSettings;
pub use interpreter::RuntimeError;
pub use name_resolution::NameResolutionError;
pub use parser::ParseError;
pub use registry::BaseRepresentation;
pub use registry::BaseRepresentationFactor;
pub use typed_ast::Statement;
pub use typed_ast::Type;
use unit::BaseUnitAndFactor;
use unit_registry::UnitMetadata;

use crate::prefix_parser::PrefixParserResult;
use crate::unicode_input::UNICODE_INPUT;

pub use compact_str;

#[derive(Debug, Clone, Error)]
pub enum NumbatError {
    #[error("{0}")]
    ResolverError(ResolverError),
    #[error("{0}")]
    NameResolutionError(NameResolutionError),
    #[error("{0}")]
    TypeCheckError(TypeCheckError),
    #[error("{0}")]
    RuntimeError(RuntimeError),
}

type Result<T> = std::result::Result<T, Box<NumbatError>>;

#[derive(Clone)]
pub struct Context {
    prefix_transformer: Transformer,
    typechecker: TypeChecker,
    interpreter: BytecodeInterpreter,
    resolver: Resolver,
    load_currency_module_on_demand: bool,
    terminal_width: Option<usize>,
}

impl Context {
    pub fn new(module_importer: impl ModuleImporter + 'static) -> Self {
        Context {
            prefix_transformer: Transformer::new(),
            typechecker: TypeChecker::default(),
            interpreter: BytecodeInterpreter::new(),
            resolver: Resolver::new(module_importer),
            load_currency_module_on_demand: false,
            terminal_width: None,
        }
    }

    pub fn new_without_importer() -> Self {
        Self::new(NullImporter::default())
    }

    pub fn set_debug(&mut self, activate: bool) {
        self.interpreter.set_debug(activate);
    }

    pub fn load_currency_module_on_demand(&mut self, yes: bool) {
        self.load_currency_module_on_demand = yes;
    }

    /// Fill the currency exchange rate cache. This call is blocking.
    pub fn prefetch_exchange_rates() {
        let _unused = ExchangeRatesCache::fetch();
    }

    pub fn set_exchange_rates(xml_content: &str) {
        ExchangeRatesCache::set_from_xml(xml_content);
    }

    pub fn use_test_exchange_rates() {
        ExchangeRatesCache::use_test_rates();
    }

    pub fn variable_names(&self) -> impl Iterator<Item = CompactString> + '_ {
        self.prefix_transformer
            .variable_names
            .iter()
            .filter(|name| !name.starts_with('_'))
            .cloned()
    }

    pub fn function_names(&self) -> impl Iterator<Item = CompactString> + '_ {
        self.prefix_transformer
            .function_names
            .iter()
            .filter(|name| !name.starts_with('_'))
            .cloned()
    }

    pub fn functions(
        &self,
    ) -> impl Iterator<
        Item = (
            CompactString,
            Option<CompactString>,
            CompactString,
            Option<CompactString>,
            Option<CompactString>,
            Vec<(CompactString, Option<CompactString>)>,
            CodeSource,
        ),
    > + '_ {
        self.prefix_transformer
            .function_names
            .iter()
            .filter(|name| !name.starts_with('_'))
            .map(move |name| {
                let (signature, meta) = self.typechecker.lookup_function(name).unwrap();
                (
                    name.clone(),
                    meta.name.clone(),
                    signature
                        .pretty_print(self.dimension_registry())
                        .to_compact_string(),
                    meta.description.clone(),
                    meta.url.clone(),
                    meta.examples.clone(),
                    self.resolver
                        .get_code_source(signature.definition_span.code_source_id),
                )
            })
    }

    pub fn unit_names(&self) -> &[Vec<CompactString>] {
        &self.prefix_transformer.unit_names
    }

    pub fn dimension_names(&self) -> &[CompactString] {
        &self.prefix_transformer.dimension_names
    }

    pub fn print_environment(&self) -> Markup {
        let mut output = m::empty();

        output += m::emphasized("List of functions:") + m::nl();
        output += self.print_functions() + m::nl();

        output += m::emphasized("List of dimensions:") + m::nl();
        output += self.print_dimensions() + m::nl();

        output += m::emphasized("List of units:") + m::nl();
        output += self.print_units() + m::nl();

        output += m::emphasized("List of variables:") + m::nl();
        output += self.print_variables() + m::nl();

        output
    }

    fn print_sorted(&self, mut entries: Vec<CompactString>, format_type: FormatType) -> Markup {
        entries.sort_by_key(|e| e.to_lowercase());

        let formatter = ColumnFormatter::new(self.terminal_width.unwrap_or(80));
        formatter.format(entries, format_type)
    }

    pub fn print_functions(&self) -> Markup {
        self.print_sorted(self.function_names().collect(), FormatType::Identifier)
    }

    pub fn print_dimensions(&self) -> Markup {
        self.print_sorted(self.dimension_names().into(), FormatType::TypeIdentifier)
    }

    pub fn print_variables(&self) -> Markup {
        self.print_sorted(self.variable_names().collect(), FormatType::Identifier)
    }

    pub fn print_units(&self) -> Markup {
        let units = self.unit_names().iter().flatten().cloned().collect();
        self.print_sorted(units, FormatType::Unit)
    }

    /// Gets completions for the given word_part
    ///
    /// If `add_paren` is true, then an opening paren will be added to the end of function names
    pub fn get_completions_for(
        &self,
        word_part: &str,
        add_paren: bool,
    ) -> impl Iterator<Item = String> {
        const COMMON_METRIC_PREFIXES: &[&str] = &[
            "pico", "nano", "micro", "milli", "centi", "kilo", "mega", "giga", "tera",
        ];

        let metric_prefixes: Vec<_> = COMMON_METRIC_PREFIXES
            .iter()
            .filter(|prefix| {
                word_part.starts_with(*prefix)
                    || (!word_part.is_empty() && prefix.starts_with(word_part))
            })
            .collect();

        let mut words = Vec::new();

        let mut add_if_valid = |word: Cow<'_, str>| {
            if word.starts_with(word_part) {
                words.push(word.into_owned());
            }
        };

        for kw in KEYWORDS {
            add_if_valid((*kw).into());
        }

        for (patterns, _) in UNICODE_INPUT {
            for pattern in *patterns {
                add_if_valid((*pattern).into());
            }
        }

        for variable in self.variable_names() {
            add_if_valid(variable.into());
        }

        for mut function in self.function_names() {
            if add_paren {
                function.push('(');
            }
            add_if_valid(function.into());
        }

        for dimension in self.dimension_names() {
            add_if_valid(dimension.into());
        }

        for (_, (_, meta)) in self.unit_representations() {
            for (unit, accepts_prefix) in meta.aliases {
                // Add some of the common long prefixes for units that accept them.
                // We do not add all possible prefixes here in order to keep the
                // number of completions to a reasonable size. Also, we do not add
                // short prefixes for units that accept them, as that leads to lots
                // and lots of 2-3 character words.
                if accepts_prefix.long && meta.metric_prefixes {
                    for prefix in &metric_prefixes {
                        add_if_valid(format!("{prefix}{unit}").into());
                    }
                }

                add_if_valid(unit.into());
            }
        }

        words.sort();
        words.dedup();

        words.into_iter()
    }

    pub fn print_info_for_keyword(&mut self, keyword: &str) -> Markup {
        fn url_encode(s: &str) -> CompactString {
            let mut out = CompactString::with_capacity(s.len());
            for c in s.chars() {
                match c {
                    '(' => out.push_str("%28"),
                    ')' => out.push_str("%29"),
                    _ => out.push(c),
                }
            }
            out
        }

        if keyword.is_empty() {
            return m::text("Usage: info <unit or variable>");
        }
        let reg = self.interpreter.get_unit_registry();

        if let PrefixParserResult::UnitIdentifier(_span, prefix, _, full_name) =
            self.prefix_transformer.prefix_parser.parse(keyword)
        {
            if let Some(md) = reg
                .inner
                .get_base_representation_for_name(&full_name)
                .ok()
                .map(|(_, md)| md)
            {
                let mut help = m::text("Unit: ")
                    + m::unit(md.name.unwrap_or_else(|| keyword.to_compact_string()));
                if let Some(url) = &md.url {
                    help += m::text(" (") + m::string(url_encode(url)) + m::text(")");
                }
                help += m::nl();
                if md.aliases.len() > 1 {
                    help += m::text("Aliases: ")
                        + m::text(
                            md.aliases
                                .iter()
                                .map(|(x, _)| x.as_str())
                                .collect::<Vec<_>>()
                                .join_compact(", "),
                        )
                        + m::nl();
                }

                if let Some(description) = &md.description {
                    let desc = "Description: ";
                    let mut lines = description.lines();
                    help += m::text(desc)
                        + m::text(
                            lines
                                .by_ref()
                                .next()
                                .unwrap_or("")
                                .trim()
                                .to_compact_string(),
                        )
                        + m::nl();

                    for line in lines {
                        help += m::whitespace(CompactString::const_new(" ").repeat(desc.len()))
                            + m::text(line.trim().to_compact_string())
                            + m::nl();
                    }
                }

                if matches!(md.type_, Type::Dimension(d) if d.is_scalar()) {
                    help += m::text("A dimensionless unit ([")
                        + md.readable_type
                        + m::text("])")
                        + m::nl();
                } else {
                    help += m::text("A unit of: ") + md.readable_type + m::nl();
                }

                if let Some(defining_info) = self.interpreter.get_defining_unit(&full_name) {
                    let x = defining_info
                        .iter()
                        .filter(|u| !u.unit_id.is_base())
                        .map(|unit_factor| unit_factor.unit_id.unit_and_factor())
                        .next();

                    if !prefix.is_none() {
                        help += m::nl()
                            + m::value("1 ")
                            + m::unit(keyword.to_compact_string())
                            + m::text(" = ")
                            + m::value(prefix.factor().pretty_print())
                            + m::space()
                            + m::unit(full_name.to_compact_string());
                    }

                    if let Some(BaseUnitAndFactor(prod, num)) = x {
                        help += m::nl()
                            + m::value("1 ")
                            + m::unit(full_name.to_compact_string())
                            + m::text(" = ")
                            + m::value(num.pretty_print())
                            + m::space()
                            + prod.pretty_print_with(
                                |f| f.exponent,
                                'x',
                                '/',
                                true,
                                Some(m::FormatType::Unit),
                            );
                    } else {
                        help += m::nl()
                            + m::unit(full_name.to_compact_string())
                            + m::text(" is a base unit");
                    }
                };

                help += m::nl();

                return help;
            }
        };

        if let Some(l) = self.interpreter.lookup_global(keyword) {
            let mut help = m::text("Variable: ");
            if let Some(name) = &l.metadata.name {
                help += m::text(name.clone());
            } else {
                help += m::identifier(keyword.to_compact_string());
            }
            if let Some(url) = &l.metadata.url {
                help += m::text(" (") + m::string(url_encode(url)) + m::text(")");
            }
            help += m::nl();

            if let Some(description) = &l.metadata.description {
                let desc = "Description: ";
                let mut lines = description.lines();
                help += m::text(desc)
                    + m::text(
                        lines
                            .by_ref()
                            .next()
                            .unwrap_or("")
                            .trim()
                            .to_compact_string(),
                    )
                    + m::nl();

                for line in lines {
                    help += m::whitespace(CompactString::const_new(" ").repeat(desc.len()))
                        + m::text(line.trim().to_compact_string())
                        + m::nl();
                }
            }

            if l.metadata.aliases.len() > 1 {
                help += m::text("Aliases: ")
                    + m::text(
                        l.metadata
                            .aliases
                            .iter()
                            .map(|x| x.as_str())
                            .collect::<Vec<_>>()
                            .join_compact(", "),
                    )
                    + m::nl();
            }

            if let Ok((_, results)) = self.interpret(keyword, CodeSource::Internal) {
                help += m::nl() + results.to_markup(None, self.dimension_registry(), true, true);
            }

            return help;
        }

        if let Some((fn_signature, fn_metadata)) = self.typechecker.lookup_function(keyword) {
            let metadata = fn_metadata.clone();

            let mut help = m::text("Function:    ");
            if let Some(name) = &metadata.name {
                help += m::text(name.clone());
            } else {
                help += m::identifier(keyword.to_compact_string());
            }
            if let Some(url) = &metadata.url {
                help += m::text(" (") + m::string(url_encode(url)) + m::text(")");
            }
            help += m::nl();

            help += m::text("Signature:  ")
                + m::space()
                + fn_signature.pretty_print(self.typechecker.registry())
                + m::nl();

            if let Some(description) = &metadata.description {
                let desc = "Description: ";
                let mut lines = description.lines();
                help += m::text(desc)
                    + m::text(
                        lines
                            .by_ref()
                            .next()
                            .unwrap_or("")
                            .trim()
                            .to_compact_string(),
                    )
                    + m::nl();

                for line in lines {
                    help += m::whitespace(CompactString::new(" ").repeat(desc.len()))
                        + m::text(line.trim().to_compact_string())
                        + m::nl();
                }
            }

            return help;
        }

        m::text("Not found")
    }

    pub fn list_modules(&self) -> impl Iterator<Item = CompactString> {
        let modules = self.resolver.get_importer().list_modules();
        modules.into_iter().map(|m| m.0.join_compact("::"))
    }

    pub fn dimension_registry(&self) -> &DimensionRegistry {
        self.typechecker.registry()
    }

    pub fn base_units(&self) -> impl Iterator<Item = CompactString> + '_ {
        self.interpreter
            .get_unit_registry()
            .inner
            .iter_base_entries()
    }

    pub fn unit_representations(
        &self,
    ) -> impl Iterator<Item = (CompactString, (BaseRepresentation, UnitMetadata))> + '_ {
        let registry = self.interpreter.get_unit_registry();

        let unit_names = registry
            .inner
            .iter_base_entries()
            .chain(registry.inner.iter_derived_entries());

        unit_names.map(|unit_name| {
            let info = registry
                .inner
                .get_base_representation_for_name(&unit_name)
                .unwrap();
            (unit_name, info)
        })
    }

    pub fn resolver(&self) -> &Resolver {
        &self.resolver
    }

    pub fn resolver_mut(&mut self) -> &mut Resolver {
        &mut self.resolver
    }

    pub fn interpret<'a>(
        &mut self,
        code: &'a str,
        code_source: CodeSource,
    ) -> Result<(Vec<typed_ast::Statement<'a>>, InterpreterResult)> {
        self.interpret_with_settings(&mut InterpreterSettings::default(), code, code_source)
    }

    pub fn interpret_with_settings<'a>(
        &mut self,
        settings: &mut InterpreterSettings,
        code: &'a str,
        code_source: CodeSource,
    ) -> Result<(Vec<typed_ast::Statement<'a>>, InterpreterResult)> {
        let statements = self
            .resolver
            .resolve(code, code_source.clone())
            .map_err(NumbatError::ResolverError)?;

        let prefix_transformer_old = self.prefix_transformer.clone();

        let result = self
            .prefix_transformer
            .transform(statements)
            .map_err(NumbatError::NameResolutionError);

        if result.is_err() {
            // Reset the state of the prefix transformer to what we had before. This is necessary
            // for REPL use cases where we want to back track from type-check errors.
            // For example:
            //
            //     >>> fn f(h) = 1
            //     error: identifier clash in definition
            //         …
            //     >>> fn f(h_) = 1     # <-- here we want to use 'f' again
            //
            self.prefix_transformer = prefix_transformer_old.clone();
        }

        let transformed_statements = result?;

        let typechecker_old = self.typechecker.clone();

        let result = self
            .typechecker
            .check(&transformed_statements)
            .map_err(|err| NumbatError::TypeCheckError(*err));

        if result.is_err() {
            // Reset the state of the prefix transformer to what we had before. This is necessary
            // for REPL use cases where we want to back track from type-check errors.
            // For example:
            //
            //     >>> let x: Length = 1s      # <-- here we register the name 'x' before type checking
            //     Type check error: Incompatible dimensions in variable definition:
            //         specified dimension: Length
            //         actual dimension: Time
            //     >>> let x: Length = 1m      # <-- here we want to use the name 'x' again
            //
            self.prefix_transformer = prefix_transformer_old.clone();
            self.typechecker = typechecker_old.clone();

            if self.load_currency_module_on_demand {
                if let Err(NumbatError::TypeCheckError(TypeCheckError::UnknownIdentifier(
                    _,
                    identifier,
                    _,
                ))) = &result
                {
                    // TODO: maybe we can somehow load this list of identifiers from units::currencies?
                    const CURRENCY_IDENTIFIERS: &[&str] = &[
                        "$",
                        "USD",
                        "usd",
                        "dollar",
                        "dollars",
                        "A$",
                        "AUD",
                        "aud",
                        "australian_dollar",
                        "australian_dollars",
                        "C$",
                        "CAD",
                        "cad",
                        "canadian_dollar",
                        "canadian_dollars",
                        "CHF",
                        "chf",
                        "swiss_franc",
                        "swiss_francs",
                        "CNY",
                        "cny",
                        "yuan",
                        "renminbi",
                        "元",
                        "EUR",
                        "eur",
                        "euro",
                        "euros",
                        "€",
                        "GBP",
                        "gbp",
                        "british_pound",
                        "pound_sterling",
                        "£",
                        "JPY",
                        "jpy",
                        "yen",
                        "yens",
                        "¥",
                        "円",
                        "bulgarian_lev",
                        "bulgarian_leva",
                        "BGN",
                        "bgn",
                        "czech_koruna",
                        "czech_korunas",
                        "CZK",
                        "czk",
                        "Kč",
                        "hungarian_forint",
                        "hungarian_forints",
                        "HUF",
                        "huf",
                        "Ft",
                        "polish_zloty",
                        "polish_zlotys",
                        "PLN",
                        "pln",
                        "zł",
                        "romanian_leu",
                        "romanian_leus",
                        "RON",
                        "ron",
                        "lei",
                        "turkish_lira",
                        "turkish_liras",
                        "TRY",
                        "try",
                        "₺",
                        "brazilian_real",
                        "brazilian_reals",
                        "BRL",
                        "brl",
                        "R$",
                        "hong_kong_dollar",
                        "hong_kong_dollars",
                        "HKD",
                        "hkd",
                        "HK$",
                        "hk$",
                        "indonesian_rupiah",
                        "indonesian_rupiahs",
                        "IDR",
                        "idr",
                        "Rp",
                        "indian_rupee",
                        "indian_rupees",
                        "INR",
                        "inr",
                        "₹",
                        "south_korean_won",
                        "south_korean_wons",
                        "KRW",
                        "krw",
                        "₩",
                        "malaysian_ringgit",
                        "malaysian_ringgits",
                        "MYR",
                        "RM",
                        "new_zealand_dollar",
                        "new_zealand_dollars",
                        "NZD",
                        "nzd",
                        "NZ$",
                        "nz$",
                        "philippine_peso",
                        "philippine_pesos",
                        "PHP",
                        "php",
                        "₱",
                        "singapore_dollar",
                        "singapore_dollars",
                        "SGD",
                        "sgd",
                        "S$",
                        "thai_baht",
                        "thai_bahts",
                        "THB",
                        "thb",
                        "฿",
                        "danish_krone",
                        "danish_kroner",
                        "DKK",
                        "dkk",
                        "swedish_krona",
                        "swedish_kronor",
                        "SEK",
                        "sek",
                        "icelandic_króna",
                        "icelandic_krónur",
                        "ISK",
                        "isk",
                        "norwegian_krone",
                        "norwegian_kroner",
                        "NOK",
                        "nok",
                        "israeli_new_shekel",
                        "israeli_new_shekels",
                        "ILS",
                        "ils",
                        "₪",
                        "NIS",
                        "nis",
                        "south_african_rand",
                        "ZAR",
                        "zar",
                    ];
                    if CURRENCY_IDENTIFIERS.contains(&identifier.as_str()) {
                        let mut no_print_settings = InterpreterSettings {
                            print_fn: Box::new(
                                move |_: &m::Markup| { // ignore any print statements when loading this module asynchronously
                                },
                            ),
                        };

                        // We also call this from a thread at program startup, so if a user only starts
                        // to use currencies later on, this will already be available and return immediately.
                        // Otherwise, we fetch it now and make sure to block on this call.
                        {
                            let erc = ExchangeRatesCache::fetch();

                            if erc.is_none() {
                                return Err(Box::new(NumbatError::RuntimeError(
                                    RuntimeError::CouldNotLoadExchangeRates,
                                )));
                            }
                        }

                        let _ = self.interpret_with_settings(
                            &mut no_print_settings,
                            "use units::currencies",
                            CodeSource::Internal,
                        )?;

                        // Make sure we do not run into an infinite loop in case loading that
                        // module did not bring in the required currency unit identifier. This
                        // can happen if the list of currency identifiers is not in sync with
                        // what the module actually defines.
                        self.load_currency_module_on_demand = false;

                        // Now we try to evaluate the user expression again:
                        return self.interpret_with_settings(settings, code, code_source);
                    }
                }
            }
        }

        let typed_statements = result?;

        let interpreter_old = self.interpreter.clone();

        let result = self.interpreter.interpret_statements(
            settings,
            &typed_statements,
            self.typechecker.registry(),
        );

        if result.is_err() {
            // Similar to above: we need to reset the state of the typechecker and the prefix transformer
            // here for REPL use cases like:
            //
            //    >>> let q = 1 / 0
            //    error: runtime error
            //     = Division by zero
            //
            //    -> 'q' should not be defined, so 'q' properly leads to a "unknown identifier" error
            //       and another 'let q = …' works as intended.
            //
            self.prefix_transformer = prefix_transformer_old;
            self.typechecker = typechecker_old;
            self.interpreter = interpreter_old;
        }

        let result = result.map_err(|err| NumbatError::RuntimeError(*err))?;

        Ok((typed_statements, result))
    }

    pub fn print_diagnostic(&self, error: impl ErrorDiagnostic) {
        use codespan_reporting::term::{
            self,
            termcolor::{ColorChoice, StandardStream},
            Config,
        };

        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = Config::default();

        // we want to be sure no one can write between our diagnostics
        let mut writer = writer.lock();
        for diagnostic in error.diagnostics() {
            term::emit(&mut writer, &config, &self.resolver.files, &diagnostic).unwrap();
        }
    }

    pub fn set_terminal_width(&mut self, width: Option<usize>) {
        self.terminal_width = width;
    }
}
