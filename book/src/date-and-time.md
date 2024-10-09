# Date and time

Numbat supports date and time handling based on the [proleptic Gregorian calendar](https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar),
which is the (usual) Gregorian calendar extended to dates before its introduction in 1582.

A few examples of useful operations that can be performed on dates and times:

```nbt
# How many days are left until September 1st?
date("2024-11-01") - today() -> days

# What time is it in Nepal right now?
now() -> tz("Asia/Kathmandu")  # use tab completion to find time zone names

# What is the local time when it is 2024-11-01 12:30:00 in Australia?
datetime("2024-11-01 12:30:00 Australia/Sydney") -> local

# Which date was 1 million seconds ago?
now() - 1 million seconds

# Which date is 40 days from now?
calendar_add(now(), 40 days)

# Which weekday was the 1st day of this century?
date("2000-01-01") -> weekday

# What is the current UNIX timestamp?
now() -> unixtime

# What is the date corresponding to a given UNIX timestamp?
from_unixtime(1707568901)

# How long are one million seconds in years, months, days, hours, minutes, seconds?
1 million seconds -> human
```

## Date and time arithmetic

The following operations are supported for `DateTime` objects:

| Left | Operator | Right | Result |
| ---- | -------- | ----- | ------ |
| `DateTime` | `-` | `DateTime` | Duration between the two dates as a `Time`. In `seconds`, by default. Use normal conversion for other time units. |
| `DateTime` | `+` | `Time` | New `DateTime` by adding the duration to the date |
| `DateTime` | `-` | `Time` | New `DateTime` by subtracting the duration from the date |
| `DateTime` | `->` | `tz("…")` | Converts the datetime to the specified time zone. Note that you can use tab-completion for time zone names. |

<div class="warning">

**Warning**: You can directly add `days`, `months` and `years` to a given date (`now() + 3 months`), but note that the result might not be what you expect.
The unit `day` is defined as having a length of 24 hours. But due to daylight
saving time, days can be shorter or longer than that. A `month` is defined
as 1/12 of a `year`, but calendar months have varying lengths. And a `year`
is defined as the average length of a
[tropical](https://en.wikipedia.org/wiki/Tropical_year) year. But a calendar
year can have 365 or 366 days, depending on whether it is a leap year or not.

If you want to take all of these factors into account, you should use the `calendar_add`/`calendar_sub` functions instead of directly adding or
subtracting `days`, `months`, or `years`.

</div>

## Date, time, and duration functions

The following functions are available for date and time handling:

- `now() -> DateTime`: Returns the current date and time.
- `today() -> DateTime`: Returns the current date at midnight (in the local time).
- `datetime(input: String) -> DateTime`: Parses a string (date and time) into a `DateTime` object.
- `date(input: String) -> DateTime`: Parses a string (only date) into a `DateTime` object.
- `time(input: String) -> DateTime`: Parses a string (only time) into a `DateTime` object.
- `format_datetime(format: String, dt: DateTime) -> String`: Formats a `DateTime` object as a string. See [this page](https://docs.rs/jiff/latest/jiff/fmt/strtime/index.html#conversion-specifications) for possible format specifiers.
- `tz(tz: String) -> Fn[(DateTime) -> DateTime]`: Returns a timezone conversion function, typically used with the conversion operator (`datetime -> tz("Europe/Berlin")`)
- `local(dt: DateTime) -> DateTime`: Timezone conversion function targeting the users local timezone (`datetime -> local`)
- `get_local_timezone() -> String`: Returns the users local timezone
- `unixtime(dt: DateTime) -> Scalar`: Converts a `DateTime` to a UNIX timestamp.
- `from_unixtime(ut: Scalar) -> DateTime`: Converts a UNIX timestamp to a `DateTime` object.
- `calendar_add(dt: DateTime, span: Time)`: Add a span of time to a `DateTime` object, taking proper calendar arithmetic into accound.
- `calendar_sub(dt: DateTime, span: Time)`: Subtract a span of time from a `DateTime` object, taking proper calendar arithmetic into accound.
- `weekday(dt: DateTime) -> String`: Returns the weekday of a `DateTime` object as a string.
- `human(duration: Time) -> String`: Converts a `Time` to a human-readable string in days, hours, minutes and seconds.
- `julian_date(dt: DateTime) -> Scalar`: Convert a `DateTime` to a [Julian date](https://en.wikipedia.org/wiki/Julian_day).

## Date time formats

The following formats are supported by `datetime`. UTC offsets are mandatory for the RFC 3339 and
RFC 2822 formats. The other formats can optionally include a time zone name or UTC offset. If no time
zone is specified, the local time zone is used.

| Format | Examples |
| ------ | ------- |
| [RFC 3339](https://tools.ietf.org/html/rfc3339) | `2024-02-10T12:30:00Z`<br>`2024-02-10T06:30:00-06:00` |
| [RFC 2822](https://tools.ietf.org/html/rfc2822) | `Sat, 10 Feb 2024 12:30:00 Z`<br>`Sat, 10 Feb 2024 06:30:00 -0600` |
| `%Y-%m-%d %H:%M:%S%.f` | `2024-02-10 12:30:00`<br>`2024-02-10 06:30:00 -0600`<br>`2024-02-10 07:30:00 US/Eastern`<br>`2024-02-10 12:30:00.123456` |
| `%Y/%m/%d %H:%M:%S%.f` | same, but with `/` separator |
| `%Y-%m-%d %H:%M` | `2024-02-10 12:30`<br>`2024-02-10 06:30 -0600`<br>`2024-02-10 07:30 US/Eastern` |
| `%Y/%m/%d %H:%M` | same, but with `/` separator |
| `%Y-%m-%d %I:%M:%S%.f %p` | `2024-02-10 12:30:00 PM`<br>`2024-02-10 06:30:00 AM -0600`<br>`2024-02-10 07:30:00 AM US/Eastern`<br>`2024-02-10 12:30:00.123456 PM` |
| `%Y/%m/%d %I:%M:%S%.f %p` | same, but with `/` separator |
| `%Y-%m-%d %I:%M %p` | `2024-02-10 12:30 PM`<br>`2024-02-10 06:30 AM -0600`<br>`2024-02-10 07:30 AM US/Eastern` |
| `%Y/%m/%d %I:%M %p` | same, but with `/` separator |

The `date` function supports the following formats. It returns a `DateTime` object with the time set to midnight in the
specified timezone (or the local timezone if no timezone is specified).

| Format | Examples |
| ------ | ------- |
| `%Y-%m-%d` | `2024-02-10`<br>`2024-02-10 +0100`<br>`2024-02-10 Europe/Berlin` |
| `%Y/%m/%d` | `2024/02/10`<br>`2024/02/10 +0100`<br>`2024/02/10 Europe/Berlin` |

The `time` function supports the following formats. It returns a `DateTime` object with the date set to the current date.
If no timezone is specified, the local timezone is used.

| Format | Examples |
| ------ | ------- |
| `%H:%M:%S%.f` | `12:30:00`<br>`06:30:00 -0600`<br>`07:30:00 US/Eastern`<br>`12:30:00.123456` |
| `%H:%M` | `12:30`<br>`06:30 -0600`<br>`07:30 US/Eastern` |
| `%I:%M:%S%.f %p` | `12:30:00 PM`<br>`06:30:00 AM -0600`<br>`07:30:00 AM US/Eastern`<br>`12:30:00.123456 PM` |
| `%I:%M %p` | `12:30 PM`<br>`06:30 AM -0600`<br>`07:30 AM US/Eastern` |
