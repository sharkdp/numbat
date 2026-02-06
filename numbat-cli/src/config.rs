use clap::ValueEnum;
use numbat::compact_str::CompactString;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Eq, Default, Debug, Clone, Copy, ValueEnum)]
#[serde(rename_all = "kebab-case")]
pub enum IntroBanner {
    #[default]
    Long,
    Short,
    Off,
}

#[derive(Debug, Clone, Copy, PartialEq, ValueEnum, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum PrettyPrintMode {
    Always,
    Never,
    Auto,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Default)]
#[serde(rename_all = "kebab-case")]
pub enum ExchangeRateFetchingPolicy {
    /// Always fetch exchange rates in the background when the application is started
    #[default]
    OnStartup,

    /// Fetch exchange rates when a currency symbol is used
    OnFirstUse,

    /// Never fetch exchange rates
    Never,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", default, deny_unknown_fields)]
#[derive(Default)]
pub struct ExchangeRateConfig {
    pub fetching_policy: ExchangeRateFetchingPolicy,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", default, deny_unknown_fields)]
pub struct FormattingConfig {
    /// Digit separator for large integers (e.g., "_" or ","). Empty to disable.
    pub digit_separator: CompactString,

    /// Minimum number of digits before adding separators.
    pub digit_grouping_threshold: usize,

    /// Maximum significant digits for floating-point display.
    pub significant_digits: usize,

    /// The strftime format string for DateTime values.
    pub datetime: CompactString,
}

impl Default for FormattingConfig {
    fn default() -> Self {
        Self {
            digit_separator: CompactString::const_new("_"),
            digit_grouping_threshold: 6,
            significant_digits: 6,
            datetime: CompactString::const_new(numbat::datetime::DEFAULT_DATETIME_FORMAT),
        }
    }
}

impl FormattingConfig {
    pub fn to_format_options(&self) -> numbat::FormatOptions {
        numbat::FormatOptions {
            digit_separator: self.digit_separator.to_string(),
            digit_grouping_threshold: self.digit_grouping_threshold,
            significant_digits: self.significant_digits,
            datetime_format: self.datetime.to_string(),
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Default, Debug, Clone, ValueEnum)]
#[serde(rename_all = "kebab-case")]
pub enum ColorMode {
    Always,
    Never,
    #[default]
    Auto,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Default, Debug, Clone, Copy, ValueEnum)]
#[serde(rename_all = "kebab-case")]
pub enum EditMode {
    #[default]
    Emacs,
    Vi,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", default, deny_unknown_fields)]
pub struct Config {
    pub intro_banner: IntroBanner,
    pub prompt: CompactString,
    pub pretty_print: PrettyPrintMode,
    pub color: ColorMode,
    pub edit_mode: EditMode,

    #[serde(skip)]
    pub enter_repl: bool,

    #[serde(skip_serializing)]
    pub load_prelude: bool,

    #[serde(skip_serializing)]
    pub load_user_init: bool,

    pub formatting: FormattingConfig,
    pub exchange_rates: ExchangeRateConfig,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            prompt: CompactString::const_new(">>> "),
            intro_banner: IntroBanner::default(),
            pretty_print: PrettyPrintMode::Auto,
            color: ColorMode::default(),
            edit_mode: EditMode::default(),
            load_prelude: true,
            load_user_init: true,
            formatting: FormattingConfig::default(),
            exchange_rates: Default::default(),
            enter_repl: true,
        }
    }
}
