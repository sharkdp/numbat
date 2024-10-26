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

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=datetime%28%222022%2D07%2D20T21%3A52%2B0200%22%29')""></button></div><code class="language-nbt hljs numbat">datetime("2022-07-20T21:52+0200")

    = 2022-07-20 19:52:00 UTC    [DateTime]
</code></pre>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=datetime%28%222022%2D07%2D20%2021%3A52%20Europe%2FBerlin%22%29')""></button></div><code class="language-nbt hljs numbat">datetime("2022-07-20 21:52 Europe/Berlin")

    = 2022-07-20 21:52:00 CEST (UTC +02), Europe/Berlin    [DateTime]
</code></pre>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=datetime%28%222022%2F07%2F20%2009%3A52%20PM%20%2B0200%22%29')""></button></div><code class="language-nbt hljs numbat">datetime("2022/07/20 09:52 PM +0200")

    = 2022-07-20 21:52:00 (UTC +02)    [DateTime]
</code></pre>

</details>

### `format_datetime`
Formats a `DateTime` object as a string.

```nbt
fn format_datetime(format: String, input: DateTime) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=format%5Fdatetime%28%22This%20is%20a%20date%20in%20%25B%20in%20the%20year%20%25Y%2E%22%2C%20datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%29')""></button></div><code class="language-nbt hljs numbat">format_datetime("This is a date in %B in the year %Y.", datetime("2022-07-20 21:52 +0200"))

    = "This is a date in July in the year 2022."    [String]
</code></pre>

</details>

### `get_local_timezone`
Returns the users local timezone.

```nbt
fn get_local_timezone() -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=get%5Flocal%5Ftimezone%28%29')""></button></div><code class="language-nbt hljs numbat">get_local_timezone()

    = "UTC"    [String]
</code></pre>

</details>

### `tz`
Returns a timezone conversion function, typically used with the conversion operator.

```nbt
fn tz(tz: String) -> Fn[(DateTime) -> DateTime]
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%20%2D%3E%20tz%28%22Europe%2FAmsterdam%22%29')""></button></div><code class="language-nbt hljs numbat">datetime("2022-07-20 21:52 +0200") -> tz("Europe/Amsterdam")

    = 2022-07-20 21:52:00 CEST (UTC +02), Europe/Amsterdam    [DateTime]
</code></pre>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%20%2D%3E%20tz%28%22Asia%2FTaipei%22%29')""></button></div><code class="language-nbt hljs numbat">datetime("2022-07-20 21:52 +0200") -> tz("Asia/Taipei")

    = 2022-07-21 03:52:00 CST (UTC +08), Asia/Taipei    [DateTime]
</code></pre>

</details>

### `unixtime`
Converts a `DateTime` to a UNIX timestamp. Can be used on the right hand side of a conversion operator: `now() -> unixtime`.

```nbt
fn unixtime(input: DateTime) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%20%2D%3E%20unixtime')""></button></div><code class="language-nbt hljs numbat">datetime("2022-07-20 21:52 +0200") -> unixtime

    = 1_658_346_720
</code></pre>

</details>

### `from_unixtime`
Converts a UNIX timestamp to a `DateTime` object.

```nbt
fn from_unixtime(input: Scalar) -> DateTime
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=from%5Funixtime%282%5E31%29')""></button></div><code class="language-nbt hljs numbat">from_unixtime(2^31)

    = 2038-01-19 03:14:08 UTC    [DateTime]
</code></pre>

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

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=date%28%222022%2D07%2D20%22%29')""></button></div><code class="language-nbt hljs numbat">date("2022-07-20")

    = 2022-07-20 00:00:00 UTC    [DateTime]
</code></pre>

</details>

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

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=calendar%5Fadd%28datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%2C%202%20years%29')""></button></div><code class="language-nbt hljs numbat">calendar_add(datetime("2022-07-20 21:52 +0200"), 2 years)

    = 2024-07-20 21:52:00 (UTC +02)    [DateTime]
</code></pre>

</details>

### `calendar_sub`
Subtract the given time span from a `DateTime`. This uses leap-year and DST-aware calendar arithmetic with variable-length days, months, and years.

```nbt
fn calendar_sub(dt: DateTime, span: Time) -> DateTime
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=calendar%5Fsub%28datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%2C%203%20years%29')""></button></div><code class="language-nbt hljs numbat">calendar_sub(datetime("2022-07-20 21:52 +0200"), 3 years)

    = 2019-07-20 21:52:00 (UTC +02)    [DateTime]
</code></pre>

</details>

### `weekday`
Get the day of the week from a given `DateTime`.

```nbt
fn weekday(dt: DateTime) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=weekday%28datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%29')""></button></div><code class="language-nbt hljs numbat">weekday(datetime("2022-07-20 21:52 +0200"))

    = "Wednesday"    [String]
</code></pre>

</details>

### `julian_date` (Julian date)
Convert a `DateTime` to a Julian date, the number of days since the origin of the Julian date system (noon on November 24, 4714 BC in the proleptic Gregorian calendar).
More information [here](https://en.wikipedia.org/wiki/Julian_day).

```nbt
fn julian_date(dt: DateTime) -> Time
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=julian%5Fdate%28datetime%28%222022%2D07%2D20%2021%3A52%20%2B0200%22%29%29')""></button></div><code class="language-nbt hljs numbat">julian_date(datetime("2022-07-20 21:52 +0200"))

    = 2.45978e+6 day    [Time]
</code></pre>

</details>

### `human` (Human-readable time duration)
Converts a time duration to a human-readable string in days, hours, minutes and seconds.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn human(time: Time) -> String
```

<details>
<summary>Examples</summary>

How long is a microcentury?
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=century%2F1e6%20%2D%3E%20human')""></button></div><code class="language-nbt hljs numbat">century/1e6 -> human

    = "52 minutes + 35.693 seconds"    [String]
</code></pre>

</details>

