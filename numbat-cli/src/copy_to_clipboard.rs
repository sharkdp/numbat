use numbat::{
    markup::Markup, num_format::CustomFormat, num_format::Grouping, pretty_dtoa::FmtFloatConfig,
    pretty_print::PrettyPrint, value::Value, FloatDisplayConfigSource, RuntimeError,
};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Eq, Default, Debug, Clone)]
#[serde(remote = "Grouping", rename_all = "kebab-case")]
pub enum IntGroupingConfig {
    #[default]
    Standard,
    Indian,
    #[serde(rename = "none")]
    Posix,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Debug, Clone)]
#[serde(rename_all = "kebab-case", default, deny_unknown_fields)]
pub struct IntDisplayConfig {
    #[serde(with = "IntGroupingConfig")]
    pub grouping: Grouping,
    pub separator: String,
    pub minus_sign: String,
}

impl Default for IntDisplayConfig {
    fn default() -> Self {
        Self {
            grouping: Grouping::Standard,
            separator: ",".into(),
            minus_sign: "-".into(),
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Debug, Clone)]
#[serde(rename_all = "kebab-case", default, deny_unknown_fields)]
pub struct FloatDisplayConfig {
    /// 0 means this value will be ignored
    pub max_sig_digits: u8,
    /// 0 means this value will be ignored
    pub min_sig_digits: u8,
    /// 0 means this value will be ignored
    pub max_width: u8,
    pub decimal: char,
    pub capitalize_e: bool,
}

impl Default for FloatDisplayConfig {
    fn default() -> Self {
        Self {
            max_sig_digits: 0,
            min_sig_digits: 0,
            max_width: 0,
            decimal: '.',
            capitalize_e: false,
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Default, Debug, Clone)]
#[serde(rename_all = "kebab-case", default, deny_unknown_fields)]
pub struct NumericDisplayConfig {
    #[serde(serialize_with = "serialize_option_config")]
    pub int_config: Option<IntDisplayConfig>,
    #[serde(serialize_with = "serialize_option_config")]
    pub float_config: Option<FloatDisplayConfig>,
}

fn serialize_option_config<S, T: Serialize + Default>(
    value: &Option<T>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    match value {
        Some(o) => o.serialize(serializer),
        None => T::default().serialize(serializer),
    }
}

pub(crate) fn pretty_print_value(
    v: &Value,
    config: &NumericDisplayConfig,
) -> Result<Markup, Box<RuntimeError>> {
    Ok(match &v {
        Value::Quantity(q) => {
            let NumericDisplayConfig {
                int_config,
                float_config,
            } = config;

            let float_options = float_config.as_ref().map(|c| {
                let FloatDisplayConfig {
                    max_sig_digits,
                    min_sig_digits,
                    max_width,
                    decimal,
                    capitalize_e,
                } = *c;

                let mut opts = FmtFloatConfig::default()
                    .radix_point(decimal)
                    .capitalize_e(capitalize_e);
                if max_sig_digits > 0 {
                    opts = opts.max_significant_digits(max_sig_digits);
                }
                if min_sig_digits > 0 {
                    opts = opts.min_significant_digits(min_sig_digits);
                }
                if max_width > 0 {
                    opts = opts.max_width(max_width);
                }

                opts
            });

            let int_options = int_config
                .as_ref()
                .map(|c| {
                    let int_config @ IntDisplayConfig {
                        grouping,
                        separator,
                        minus_sign,
                    } = c;

                    CustomFormat::builder()
                        .grouping(*grouping)
                        .separator(separator)
                        .minus_sign(minus_sign)
                        .build()
                        .map_err(|err| {
                            Box::new(RuntimeError::IntegerDisplayConfig(
                                format!("{int_config:?}"),
                                err.to_string(),
                            ))
                        })
                })
                .transpose()?;

            q.pretty_print_with_options(
                match float_options {
                    Some(o) => FloatDisplayConfigSource::UserConfig(o),
                    None => FloatDisplayConfigSource::Numbat(None),
                },
                int_options,
            )
        }

        v => v.pretty_print(),
    })
}
