---
icon: lucide/clock
---

# Date and time

See [this page](./date-and-time.md) for a general introduction to date and time handling in Numbat.

Defined in: `datetime::functions`, `datetime::human`

### `now`
Returns the current date and time.

```nbt
fn now() -> DateTime
```

### `datetime`
Parses a string (date and time) into a `DateTime` object. See [here](./date-and-time.md#date-time-formats) for an overview of the supported formats.

```nbt
fn datetime(input: String) -> DateTime
```

!!! example "Example"
    ```nbt
    datetime("2022-07-20T21:52+0200")

        = 2022-07-20 19:52:00 UTC    [DateTime]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=datetime%28%222022%2D07%2D20T21%3A52%2B0200%22%29){ .md-button }

!!! example "Example"
    ```nbt
    datetime("2022-07-20 21:52 Europe/Berlin")

        = 2022-07-20 21:52:00 CEST (UTC +02), Europe/Berlin    [DateTime]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=datetime%28%222022%2D07%2D20%2021%3A52%20Europe%2FBerlin%22%29){ .md-button }

!!! example "Example"
    ```nbt
    datetime("2022/07/20 09:52 PM +0200")

        = 2022-07-20 21:52:00 (UTC +02)    [DateTime]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=datetime%28%222022%2F07%2F20%2009%3A52%20PM%20%2B0200%22%29){ .md-button }

### `format_datetime`
Formats a `DateTime` object as a string.

```nbt
fn format_datetime(format: String, input: DateTime) -> String
```

!!! example "Example"
    ```nbt
    format_datetime("This is a date in %B in the year %Y.", datetime("2022-07-20 21:52 +0200"))

        = "This is a date in July in the year 2022."    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=format%5Fdatetime%28%22This%20is%20a%20date%20in%20%25B%20in%20the%20year%20%25Y%2E%22%2C%20datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%29){ .md-button }

### `get_local_timezone`
Returns the users local timezone.

```nbt
fn get_local_timezone() -> String
```

!!! example "Example"
    ```nbt
    get_local_timezone()

        = "UTC"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=get%5Flocal%5Ftimezone%28%29){ .md-button }

### `tz`
Returns a timezone conversion function, typically used with the conversion operator.

```nbt
fn tz(tz: String) -> Fn[(DateTime) -> DateTime]
```

!!! example "Example"
    ```nbt
    datetime("2022-07-20 21:52 +0200") -> tz("Europe/Amsterdam")

        = 2022-07-20 21:52:00 CEST (UTC +02), Europe/Amsterdam    [DateTime]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%20%2D%3E%20tz%28%22Europe%2FAmsterdam%22%29){ .md-button }

!!! example "Example"
    ```nbt
    datetime("2022-07-20 21:52 +0200") -> tz("Asia/Taipei")

        = 2022-07-21 03:52:00 CST (UTC +08), Asia/Taipei    [DateTime]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%20%2D%3E%20tz%28%22Asia%2FTaipei%22%29){ .md-button }

### `unixtime`
Converts a `DateTime` to a UNIX timestamp. Can be used on the right hand side of a conversion operator: `now() -> unixtime`.

```nbt
fn unixtime(input: DateTime) -> Scalar
```

!!! example "Example"
    ```nbt
    datetime("2022-07-20 21:52 +0200") -> unixtime

        = 1_658_346_720
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%20%2D%3E%20unixtime){ .md-button }

### `from_unixtime`
Converts a UNIX timestamp to a `DateTime` object.

```nbt
fn from_unixtime(input: Scalar) -> DateTime
```

!!! example "Example"
    ```nbt
    from_unixtime(2^31)

        = 2038-01-19 03:14:08 UTC    [DateTime]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=from%5Funixtime%282%5E31%29){ .md-button }

### `today`
Returns the current date at midnight (in the local time).

```nbt
fn today() -> DateTime
```

### `date`
Parses a string (only date) into a `DateTime` object.

```nbt
fn date(input: String) -> DateTime
```

!!! example "Example"
    ```nbt
    date("2022-07-20")

        = 2022-07-20 00:00:00 UTC    [DateTime]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=date%28%222022%2D07%2D20%22%29){ .md-button }

### `time`
Parses a string (time only) into a `DateTime` object.

```nbt
fn time(input: String) -> DateTime
```

### `calendar_add`
Adds the given time span to a `DateTime`. This uses leap-year and DST-aware calendar arithmetic with variable-length days, months, and years.

```nbt
fn calendar_add(dt: DateTime, span: Time) -> DateTime
```

!!! example "Example"
    ```nbt
    calendar_add(datetime("2022-07-20 21:52 +0200"), 2 years)

        = 2024-07-20 21:52:00 (UTC +02)    [DateTime]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=calendar%5Fadd%28datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%2C%202%20years%29){ .md-button }

### `calendar_sub`
Subtract the given time span from a `DateTime`. This uses leap-year and DST-aware calendar arithmetic with variable-length days, months, and years.

```nbt
fn calendar_sub(dt: DateTime, span: Time) -> DateTime
```

!!! example "Example"
    ```nbt
    calendar_sub(datetime("2022-07-20 21:52 +0200"), 3 years)

        = 2019-07-20 21:52:00 (UTC +02)    [DateTime]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=calendar%5Fsub%28datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%2C%203%20years%29){ .md-button }

### `weekday`
Get the day of the week from a given `DateTime`.

```nbt
fn weekday(dt: DateTime) -> String
```

!!! example "Example"
    ```nbt
    weekday(datetime("2022-07-20 21:52 +0200"))

        = "Wednesday"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=weekday%28datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%29){ .md-button }

### `julian_date` (Julian date)
Convert a `DateTime` to a Julian date, the number of days since the origin of the Julian date system (noon on November 24, 4714 BC in the proleptic Gregorian calendar).
More information [here](https://en.wikipedia.org/wiki/Julian_day).

```nbt
fn julian_date(dt: DateTime) -> Time
```

!!! example "Example"
    ```nbt
    julian_date(datetime("2022-07-20 21:52 +0200"))

        = 2.45978e+6 day    [Time]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=julian%5Fdate%28datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%29){ .md-button }

### `human` (Human-readable time duration)
Converts a time duration to a human-readable string in days, hours, minutes and seconds.
More information [here](https://numbat.dev/docs/basics/date-and-time/).

```nbt
fn human(time: Time) -> String
```

!!! example "How long is a microcentury?"
    ```nbt
    century/1e6 -> human

        = "52 minutes + 35.693 seconds"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=century%2F1e6%20%2D%3E%20human){ .md-button }

