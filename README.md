# zephyr
[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)
[![Travis Build Status](https://travis-ci.org/coot/zephyr.svg?branch=master)](https://travis-ci.org/coot/zephyr)
[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/32r7s2skrgm9ubva?svg=true)](https://ci.appveyor.com/project/coot/zephyr)

An experimental tree-shaking tool for [PureScript](https://github.com/purescript/purescript).

# Usage
```sh
# compile your project (or use `pulp build -- -g corefn`)
purs compile -g corefn bower_components/purescript-*/src/**/*.purs src/**/*.purs

# run `zephyr`
zephyr -f Main.main

# bundle your code
webpack
```

or you can bundle with `pulp`:

```sh
pulp browserify --skip-compile -o dce-output -t app.js
```

You can also specify modules as entry points, which is the same as specifying
all exported identifiers.

```sh
# include all identifiers from Data.Eq module
zephyr Data.Eq

# as above
zephyr module:Data.Eq

# include Data.Eq.Eq identifier of Data.Eq module
zephyr ident:Data.Eq.Eq

# include Data.Eq.eq identifier (not the lower case of the identifier!)
zephyr Data.Eq.eq
```

`zephyr` reads corefn json representation from `output` directory, removes non
transitive dependencies of entry points and dumps common js modules (or corefn
representation) to `dce-output` directory.

# Zephyr eval

Zephyr can evaluate some literal expressions.
```purescript
import Config (isProduction)

a = if isProduction
  then "api/prod/"
  else "api/dev/"
```
will be transformed to
```purescript
a = "api/prod/"
```
whenever `isProduction` is `true`.  This allows you to have different
development and production environment while still ship a minified code in your
production environment.  You may define `isProduction` in a module under
a `src-prod` directory and include it when compiling production code with `pulp
build -I src-prod` and to have another copy for your development environment
under `src-dev` where `isProduction` is set to `false`.

# Build & Test

```sh
cabal build exe:zephyr
```

or using nix

```sh
nix-build -A zephyr
```

To run tests

```sh
cabal run zephyr-test
```

Running test with `nix` is not supported.

# Comments

The `-f` switch is not 100% safe.  Upon running, `zephyr` will remove exports from
foreign modules that seems to be not used: are not used in purescript code and
seem not to be used in the foreign module.  If you simply assign to `exports`
using javascript dot notation then you will be fine, but if you use square
notation `exports[var]` in a dynamic way (i.e. var is a true variable rather
than a string literal), then `zephyr` might remove code that shouldn’t be
removed.

The best practice is to run a JavaScript tree-shaking algorithm via `webpack` or 
`rollup` on the JavaScript code that is pulled in your bundle by foreign imports.
