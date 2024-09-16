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

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=datetime%28%222022%2D07%2D20T21%3A52%2B0200%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> datetime("2022-07-20T21:52+0200")
    
      datetime("2022-07-20T21:52+0200")
    
        = 2022-07-20 19:52:00 UTC    [DateTime]
    
  ```
* <a href="https://numbat.dev/?q=datetime%28%222022%2D07%2D20%2021%3A52%20Europe%2FBerlin%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> datetime("2022-07-20 21:52 Europe/Berlin")
    
      datetime("2022-07-20 21:52 Europe/Berlin")
    
        = 2022-07-20 21:52:00 CEST (UTC +02), Europe/Berlin    [DateTime]
    
  ```
* <a href="https://numbat.dev/?q=datetime%28%222022%2F07%2F20%2009%3A52%20PM%20%2B0200%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> datetime("2022/07/20 09:52 PM +0200")
    
      datetime("2022/07/20 09:52 PM +0200")
    
        = 2022-07-20 21:52:00 (UTC +02)    [DateTime]
    
  ```
</details>

### `format_datetime`
Formats a `DateTime` object as a string.

```nbt
fn format_datetime(format: String, input: DateTime) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=format%5Fdatetime%28%22This%20is%20a%20date%20in%20%25B%20in%20the%20year%20%25Y%2E%22%2C%20datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> format_datetime("This is a date in %B in the year %Y.", datetime("2022-07-20 21:52 +0200"))
    
      format_datetime("This is a date in %B in the year %Y.", datetime("2022-07-20 21:52 +0200"))
    
        = "This is a date in July in the year 2022."    [String]
    
  ```
</details>

### `get_local_timezone`
Returns the users local timezone.

```nbt
fn get_local_timezone() -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=get%5Flocal%5Ftimezone%28%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> get_local_timezone()
    
      get_local_timezone()
    
        = "UTC"    [String]
    
  ```
</details>

### `tz`
Returns a timezone conversion function, typically used with the conversion operator.

```nbt
fn tz(tz: String) -> Fn[(DateTime) -> DateTime]
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%20%2D%3E%20tz%28%22Europe%2FAmsterdam%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> datetime("2022-07-20 21:52 +0200") -> tz("Europe/Amsterdam")
    
      tz("Europe/Amsterdam")(datetime("2022-07-20 21:52 +0200"))
    
        = 2022-07-20 21:52:00 CEST (UTC +02), Europe/Amsterdam    [DateTime]
    
  ```
* <a href="https://numbat.dev/?q=datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%20%2D%3E%20tz%28%22Asia%2FTaipei%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> datetime("2022-07-20 21:52 +0200") -> tz("Asia/Taipei")
    
      tz("Asia/Taipei")(datetime("2022-07-20 21:52 +0200"))
    
        = 2022-07-21 03:52:00 CST (UTC +08), Asia/Taipei    [DateTime]
    
  ```
</details>

### `unixtime`
Converts a `DateTime` to a UNIX timestamp. Can be used on the right hand side of a conversion operator: `now() -> unixtime`.

```nbt
fn unixtime(input: DateTime) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%20%2D%3E%20unixtime"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> datetime("2022-07-20 21:52 +0200") -> unixtime
    
      unixtime(datetime("2022-07-20 21:52 +0200"))
    
        = 1_658_346_720
    
  ```
</details>

### `from_unixtime`
Converts a UNIX timestamp to a `DateTime` object.

```nbt
fn from_unixtime(input: Scalar) -> DateTime
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=from%5Funixtime%282%5E31%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> from_unixtime(2^31)
    
      from_unixtime(2^31)
    
        = 2038-01-19 03:14:08 UTC    [DateTime]
    
  ```
</details>

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

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=date%28%222022%2D07%2D20%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> date("2022-07-20")
    
      date("2022-07-20")
    
        = 2022-07-20 00:00:00 UTC    [DateTime]
    
  ```
</details>

### `time`
Parses a string (time only) into a `DateTime` object.

```nbt
fn time(input: String) -> DateTime
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=time%28%2221%3A52%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> time("21:52")
    
      time("21:52")
    
        = 2024-09-16 21:52:00 UTC    [DateTime]
    
  ```
</details>

### `calendar_add`
Adds the given time span to a `DateTime`. This uses leap-year and DST-aware calendar arithmetic with variable-length days, months, and years.

```nbt
fn calendar_add(dt: DateTime, span: Time) -> DateTime
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=calendar%5Fadd%28datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%2C%202%20years%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> calendar_add(datetime("2022-07-20 21:52 +0200"), 2 years)
    
      calendar_add(datetime("2022-07-20 21:52 +0200"), 2 year)
    
        = 2024-07-20 21:52:00 (UTC +02)    [DateTime]
    
  ```
</details>

### `calendar_sub`
Subtract the given time span from a `DateTime`. This uses leap-year and DST-aware calendar arithmetic with variable-length days, months, and years.

```nbt
fn calendar_sub(dt: DateTime, span: Time) -> DateTime
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=calendar%5Fsub%28datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%2C%203%20years%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> calendar_sub(datetime("2022-07-20 21:52 +0200"), 3 years)
    
      calendar_sub(datetime("2022-07-20 21:52 +0200"), 3 year)
    
        = 2019-07-20 21:52:00 (UTC +02)    [DateTime]
    
  ```
</details>

### `weekday`
Get the day of the week from a given `DateTime`.

```nbt
fn weekday(dt: DateTime) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=weekday%28datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> weekday(datetime("2022-07-20 21:52 +0200"))
    
      weekday(datetime("2022-07-20 21:52 +0200"))
    
        = "Wednesday"    [String]
    
  ```
</details>

### `julian_date` (Julian date)
Convert a `DateTime` to a Julian date, the number of days since the origin of the Julian date system (noon on November 24, 4714 BC in the proleptic Gregorian calendar).
More information [here](https://en.wikipedia.org/wiki/Julian_day).

```nbt
fn julian_date(dt: DateTime) -> Time
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=julian%5Fdate%28datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> julian_date(datetime("2022-07-20 21:52 +0200"))
    
      julian_date(datetime("2022-07-20 21:52 +0200"))
    
        = 2.45978e+6 day    [Time]
    
  ```
</details>

### `human` (Human-readable time duration)
Converts a time duration to a human-readable string in days, hours, minutes and seconds.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn human(time: Time) -> String
```

<details>
<summary>Examples</summary>

* How long is a microcentury?

  <a href="https://numbat.dev/?q=century%2F1e6%20%2D%3E%20human"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> century/1e6 -> human
    
      human(century / 1_000_000)
    
        = "52 minutes + 35.692505184 seconds"    [String]
    
  ```
</details>

