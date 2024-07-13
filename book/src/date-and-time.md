# Date and time

Numbat supports date and time handling based on the [proleptic Gregorian calendar](https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar),
which is the (usual) Gregorian calendar extended to dates before its introduction in 1582. Julian calendar dates are currently not supported.

A few examples of useful operations that can be performed on dates and times:

```nbt
# Which date is 40 days from now?
now() + 40 days

# Which date was 1 million seconds ago?
now() - 1 million seconds

# How many days are left until September 1st?
date("2024-11-01") - today() -> days

# What time is it in Nepal right now?
now() -> tz("Asia/Kathmandu")  # use tab completion to find time zone names

# What is the local time when it is 2024-11-01 12:30:00 in Australia?
datetime("2024-11-01 12:30:00 Australia/Sydney") -> local

# What is the current UNIX timestamp?
now() -> unixtime

# What is the date corresponding to the UNIX timestamp 1707568901?
from_unixtime(1707568901)

# How long are one million seconds in years, months, days, hours, minutes, seconds
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

**Warning**: You can add `years` or `months` to a given date (`now() + 3 months`), but note that the result might not be what you expect.
The unit `year` is defined as the *average* length of a year (a [tropical year](https://en.wikipedia.org/wiki/Tropical_year), to be precise), and
`month` is defined as the *average* length of a month (1/12 of a `year`). So this does not take into account the actual length of the months or the leap years.
However, note that adding or subtracting "one year" or "one month" is not a well-defined operation anyway. For example, what should "one month after March 31st"
be? April 30th or May 1st? If your answer is April 30th, then what is "one month after March 30th"? If your answer is May 1st, then what is "one month after
April 1st"?

</div>

## Date, time, and duration functions

The following functions are available for date and time handling:

- `now() -> DateTime`: Returns the current date and time.
- `today() -> DateTime`: Returns the current date at midnight (in the local time).
- `datetime(input: String) -> DateTime`: Parses a string (date and time) into a `DateTime` object.
- `date(input: String) -> DateTime`: Parses a string (only date) into a `DateTime` object.
- `time(input: String) -> DateTime`: Parses a string (only time) into a `DateTime` object.
- `format_datetime(format: String, dt: DateTime) -> String`: Formats a `DateTime` object as a string. See [this page](https://docs.rs/chrono/latest/chrono/format/strftime/index.html#specifiers) for possible format specifiers.
- `tz(tz: String) -> Fn[(DateTime) -> DateTime]`: Returns a timezone conversion function, typically used with the conversion operator (`datetime -> tz("Europe/Berlin")`)
- `local(dt: DateTime) -> DateTime`: Timezone conversion function targeting the users local timezone (`datetime -> local`)
- `get_local_timezone() -> String`: Returns the users local timezone
- `unixtime(dt: DateTime) -> Scalar`: Converts a `DateTime` to a UNIX timestamp.
- `from_unixtime(ut: Scalar) -> DateTime`: Converts a UNIX timestamp to a `DateTime` object.
- `human(duration: Time) -> String`: Converts a `Time` to a human-readable string in days, hours, minutes and seconds

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
