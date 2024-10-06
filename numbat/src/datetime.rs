use jiff::{civil::DateTime, fmt::rfc2822, tz::TimeZone, Timestamp, Zoned};
use std::str::FromStr;

pub fn get_local_timezone_or_utc() -> TimeZone {
    TimeZone::system()
}

pub fn parse_datetime(input: &str) -> Result<Zoned, jiff::Error> {
    if let zoned @ Ok(_) = Zoned::from_str(input) {
        return zoned;
    }

    // RFC 3339
    if let Ok(timestamp) = DateTime::strptime("%Y-%m-%dT%H:%M:%S%.fZ", input) {
        if let zoned @ Ok(_) = timestamp.to_zoned(TimeZone::UTC) {
            return zoned;
        }
    }

    // RFC 2822
    if let dt @ Ok(_) = rfc2822::parse(input) {
        return dt;
    }

    const FORMATS: [&str; 8] = [
        // 24 hour formats:
        "%Y-%m-%d %H:%M:%S%.f",
        "%Y/%m/%d %H:%M:%S%.f",
        "%Y-%m-%d %H:%M",
        "%Y/%m/%d %H:%M",
        // 12 hour formats:
        "%Y-%m-%d %I:%M:%S %p%.f",
        "%Y-%m-%d %I:%M %p",
        "%Y/%m/%d %I:%M:%S %p%.f",
        "%Y/%m/%d %I:%M %p",
    ];

    for format in FORMATS {
        // Try to match the given format plus an additional UTC offset (%z)
        if let Ok(dt) = Zoned::strptime(format!("{format} %z"), input) {
            return Ok(dt);
        }

        // Try to match the given format plus an additional timezone name. This is
        // similar to '%Z', which is not supported for parsing in jiff. The reason
        // for this is that it can be ambiguous. CST, for example, has several
        // meanings.
        // We were warned by developers before us not to write timezone-related
        // code on our own, so we're probably going to regret this.

        // Get the last space-separated word in the input string, and try to parse it
        // as a timezone specifier, then try to match the rest of the string with the
        // given format.
        if let Some((rest, potential_timezone_name)) = input.rsplit_once(' ') {
            if let Ok(tz) = TimeZone::get(potential_timezone_name) {
                if let Ok(datetime) = DateTime::strptime(format, rest) {
                    return datetime.to_zoned(tz);
                }
            }
        }

        // Without timezone/offset
        if let Ok(dt) = DateTime::strptime(format, input) {
            return dt.to_zoned(get_local_timezone_or_utc());
        }
    }

    Timestamp::from_str(input).map(|ts| ts.to_zoned(get_local_timezone_or_utc()))
}

pub fn to_string(dt: &Zoned) -> String {
    let tz = dt.time_zone();

    if dt.time_zone() == &TimeZone::UTC {
        dt.strftime("%Y-%m-%d %H:%M:%S UTC").to_string()
    } else {
        let offset = dt.offset();
        let zone_abbreviation = tz.to_offset(dt.timestamp()).2;
        let abbreviation_and_offset =
            if zone_abbreviation.starts_with('+') || zone_abbreviation.starts_with('-') {
                format!("(UTC {offset})")
            } else {
                format!("{zone_abbreviation} (UTC {offset})")
            };

        let timezone_name = if let Some(iana_tz_name) = tz.iana_name() {
            format!(", {iana_tz_name}")
        } else {
            "".into()
        };

        let dt_str = dt.strftime("%Y-%m-%d %H:%M:%S");
        format!("{dt_str} {abbreviation_and_offset}{timezone_name}")
    }
}
