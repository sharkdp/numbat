# Customization

## Startup

By default, Numbat will load the following modules/files during startup, in order:

- Numbat Prelude (a module called `prelude`, either from `<module-path>/prelude.nbt` if available, or the builtin version)
- The user initialization file, if available (a file called `init.nbt` from `<config-path>/init.nbt`)

### Config path

Numbat's configuration folder (`<config-path>` above) can be found under:

|Platform|Path|
|---|---|
|Linux|`$HOME/.config/numbat` or `$XDG_CONFIG_HOME/numbat`|
|macOS|`$HOME/Library/Application Support/numbat`|
|Windows|`C:\Users\Alice\AppData\Roaming\numbat`|

## Module paths

Numbat will load modules from the following sources.
Entries higher up in the list take precedence.

* `$NUMBAT_MODULES_PATH` — If present, this environment variable can point to a single directory or contain a `:`-separated list of paths
* `<config-path>/modules` — User-customized module folder
* `/usr/share/numbat/modules` — System-wide module folder (Linux and macOS)
* `C:\Program Files\numbat\modules` — System-wide module folder (Windows)
* Builtin modules inside the `numbat` binary

## Customization

### Custom functions, constants, units

If you want to add custom functions, constants, or units to your default environment,
create a `init.nbt` file in your config folder (e.g. `~/.config/numbat/init.nbt` on Linux).

### Custom modules

You can also create your own modules that can be loaded on demand. To this end,
create a new file, say `<module-path>/user/finance.nbt` in one of the module folders
(e.g. `~/.config/numbat/modules/custom/finance.nbt` on Linux). This module can then be
loaded using

``` numbat
use custom::finance
```

in your Numbat scripts or in the REPL. You can also load custom modules from `init.nbt`
if you want to have them available all the time.

You can also organize modules into subfolders (e.g. `<module-path>/custom/finance/functions.nbt`).
In that case, you can load them using

``` numbat
use custom::finance::functions
```

In fact, the `custom` folder is just a convention to avoid name clashes with the
[standard library](https://github.com/sharkdp/numbat/tree/master/modules).
