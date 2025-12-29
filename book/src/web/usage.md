---
icon: lucide/book-open
---

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

There is a set of special commands available in the web version:

| Command | Action |
|---------|--------|
| `help`, `?` | Show a basic introduction to Numbat |
| `help commands` | Brief description of all available commands |
| `info <identifier>` | Get more information about functions, variables, units, or dimensions |
| `list` | Show all currently defined items |
| `list <what>` | Show all currently defined `functions`, `dimensions`, `variables`, or `units` |
| `clear` | Clear the console output |
| `reset` | Completely reset the interpreter state |

## Key bindings

In interactive command-line mode, you can use the following key bindings. Most importantly,
`Tab` for auto-completion, arrow keys and `Ctrl-R` for browsing the command history, and
`Ctrl-D` for exiting the interactive session.

| Key sequence | Action |
|--------------|--------|
| ++tab++ | Auto-completion |
| ++ctrl+l++ | Clear screen |
| ++up++ / ++down++ | Browse command history |
| ++ctrl+r++ | Search command history |
| ++ctrl+c++ | Clear the current line |
| ++shift+enter++ | Insert newline |
| ++home++ / ++ctrl+a++ | Move cursor to the beginning of the line |
| ++end++ / ++ctrl+e++ | Move cursor to the end of the line |
| ++ctrl+left++ / ++ctrl+right++ | Move cursor one word left/right |
| ++ctrl+k++ | Remove text to the right of the cursor |
| ++ctrl+u++ | Remove text to the left of the cursor |

## Sharing calculations

To share the result of a calculation with someone else, you can just copy the URL from
your browers address bar. As you enter new lines in the terminal, your input will be
appended to the URL to build up something like
[`https://numbat.dev/?q=let+P0+%3D+50_000+people%0A…`](https://numbat.dev/?q=let+P0+%3D+50_000+people%0Alet+growth_rate+%3D+2%25+per+year%0A%0Afn+population%28t%3A+Time%29+%3D%0A++++P0+%C3%97+e%5E%28growth_rate%C2%B7t%29+%7C%3E+round_in%28people%29%0A%0Aprint%28%22P%2820+years%29+%3D+%7Bpopulation%2820+years%29%7D%22%29%E2%8F%8E)
that you can just copy and share. To reset the state and clear the URL, use the `reset`
command (see above).
