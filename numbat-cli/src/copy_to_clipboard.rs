use numbat::{
    markup::Markup, num_format::CustomFormat, num_format::Grouping, pretty_dtoa::FmtFloatConfig,
    pretty_print::PrettyPrint, value::Value, FloatDisplayConfigSource, RuntimeError,
    UnitDisplayOptions,
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
    pub decimal: char,
    pub capitalize_e: bool,
}

impl Default for FloatDisplayConfig {
    fn default() -> Self {
        Self {
            decimal: '.',
            capitalize_e: false,
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Debug, Clone)]
#[serde(rename_all = "kebab-case", default, deny_unknown_fields)]
pub struct UnitDisplayConfig {
    fancy_exponents: bool,
    multiplication_operator: char,
    division_operator: char,
    space_btwn_operators: bool,
}

impl Default for UnitDisplayConfig {
    fn default() -> Self {
        Self {
            fancy_exponents: false,
            multiplication_operator: '·',
            division_operator: '/',
            space_btwn_operators: false,
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
    #[serde(serialize_with = "serialize_option_config")]
    pub unit_config: Option<UnitDisplayConfig>,
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
                unit_config,
            } = config;

            let float_options = float_config.as_ref().map(|c| {
                let FloatDisplayConfig {
                    decimal,
                    capitalize_e,
                } = *c;

                FmtFloatConfig::default()
                    .radix_point(decimal)
                    .capitalize_e(capitalize_e)
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

            let unit_options = unit_config.as_ref().map(|c| {
                let &UnitDisplayConfig {
                    fancy_exponents,
                    multiplication_operator,
                    division_operator,
                    space_btwn_operators,
                } = c;
                UnitDisplayOptions {
                    fancy_exponents,
                    multiplication_operator,
                    division_operator,
                    space_btwn_operators,
                }
            });

            q.pretty_print_with_options(
                match float_options {
                    Some(o) => FloatDisplayConfigSource::UserConfig(o),
                    None => FloatDisplayConfigSource::Numbat(None),
                },
                int_options,
                unit_options,
            )
        }

        v => v.pretty_print(),
    })
}
