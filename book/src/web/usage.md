# Usage

The browser-based version of Numbat is available at <https://numbat.dev/>.

## Interactive terminal

The terminal allows you to perform a sequence of calculations.
You can use the arrow keys to browse through the command history.
The special identifiers `ans` and `_` refer to the result of the last calculation. For example:

``` numbat
>>> 60 kW h / 150 kW

    = 0.4 h

>>> ans -> minutes

    = 24 min
```

## Commands

There is a set of special commands that only work in the web version:

| Command | Action |
|---------|--------|
| `list` | List all functions, dimensions, variables and units |
| `list <what>` | Where `<what>` can be `functions`, `dimensions`, `variables`, `units` |
| `info <identifier>` | Get more information about units, dimensions, variables, and functions |
| `clear` | Clear screen |
| `help`, `?` | View short help text |
| `reset` | Reset state (clear constants, functions, units, …) |

## Key bindings

In interactive command-line mode, you can use the following key bindings. Most importantly,
`Tab` for auto-completion, arrow keys and `Ctrl-R` for browsing the command history, and
`Ctrl-D` for exiting the interactive session.

| Key sequence | Action |
|--------------|--------|
| `Tab` | Auto-completion |
| `Ctrl`-`L` | Clear screen |
| `Up`, `Down` | Browse command history |
| `Ctrl`-`R` | Search command history |
| `Ctrl`-`C` | Clear the current line |
| `Shift`-`Enter` | Insert newline |
| `Home`, `Ctrl`-`A` | Move cursor to the beginning of the line |
| `End`, `Ctrl`-`E` | Move cursor to the end of the line |
| `Ctrl`-`Left`, `Ctrl`-`Right` | Move cursor one word left/right |
| `Ctrl`-`K` | Remove text to the right of the cursor |
| `Ctrl`-`U` | Remove text to the left of the cursor |

## Sharing calculations

To share the result of a calculation with someone else, you can just copy the URL from
your browers address bar. As you enter new lines in the terminal, your input will be
appended to the URL to build up something like
[`https://numbat.dev/?q=let+P0+%3D+50_000+people%0A…`](https://numbat.dev/?q=let+P0+%3D+50_000+people%0Alet+growth_rate+%3D+2%25+per+year%0A%0Afn+population(t%3A+Time)+%3D%0A++++P0+×+e^(growth_rate·t)+%7C%3E+round%0A%0Aprint("P(20+years)+%3D+{population(20+years)}"))
that you can just copy and share. To reset the state and clear the URL, use the `reset`
command (see above).
