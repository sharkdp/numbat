use chrono::{DateTime, Datelike, FixedOffset, LocalResult};
use chrono_tz::Tz;

#[cfg(feature = "local-timezone")]
pub fn get_local_timezone() -> Option<Tz> {
    let tz_str = iana_time_zone::get_timezone().ok()?;
    tz_str.parse().ok()
}

#[cfg(feature = "local-timezone")]
pub fn get_local_timezone_or_utc() -> Tz {
    get_local_timezone().unwrap_or(chrono_tz::UTC)
}

#[cfg(not(feature = "local-timezone"))]
pub fn get_local_timezone_or_utc() -> Tz {
    chrono_tz::UTC
}

pub fn parse_datetime(input: &str) -> Option<DateTime<FixedOffset>> {
    if let Ok(dt) = chrono::DateTime::parse_from_rfc3339(input) {
        Some(dt)
    } else if let Ok(dt) = chrono::DateTime::parse_from_rfc2822(input) {
        Some(dt)
    } else {
        const FORMATS: [&str; 8] = [
            // 24 hour formats:
            "%Y-%m-%d %H:%M:%S%.f",
            "%Y/%m/%d %H:%M:%S%.f",
            "%Y-%m-%d %H:%M",
            "%Y/%m/%d %H:%M",
            // 12 hour formats:
            "%Y-%m-%d %I:%M:%S%.f %p",
            "%Y-%m-%d %I:%M %p",
            "%Y/%m/%d %I:%M:%S%.f %p",
            "%Y/%m/%d %I:%M %p",
        ];

        for format in FORMATS {
            // Try to match the given format plus an additional UTC offset (%z)
            if let Ok(dt) = chrono::DateTime::parse_from_str(input, &format!("{format} %z")) {
                return Some(dt);
            }

            // Try to match the given format plus an additional timezone name (%Z).
            // chrono does not support %Z, so we implement this ourselves. We were
            // warned by developers before us not to write timezone-related code on
            // our own, so we're probably going to regret this.

            // Get the last space-separated word in the input string, and try to parse it
            // as a timezone specifier, then try to match the rest of the string with the
            // given format.
            if let Some((rest, potential_timezone_name)) = input.rsplit_once(' ') {
                if let Ok(tz) = potential_timezone_name.parse::<Tz>() {
                    if let Ok(ndt) = chrono::NaiveDateTime::parse_from_str(rest, format) {
                        if let LocalResult::Single(dt) = ndt.and_local_timezone(tz) {
                            return Some(dt.fixed_offset());
                        }
                    }
                }
            }

            // Without timezone/offset
            if let Ok(ndt) = chrono::NaiveDateTime::parse_from_str(input, format) {
                if let LocalResult::Single(dt) = ndt.and_local_timezone(get_local_timezone_or_utc())
                {
                    return Some(dt.fixed_offset());
                }
            }
        }

        None
    }
}

pub fn to_rfc2822_save(dt: &DateTime<FixedOffset>) -> String {
    if dt.year() < 0 || dt.year() > 9999 {
        "<year out of range for displaying in RFC2822>".to_string()
    } else {
        dt.to_rfc2822()
    }
}
