# zephyr
[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)
![zephyr](https://github.com/coot/zephyr/workflows/Haskell%20CI/badge.svg)

A tree-shaking tool for [PureScript](https://github.com/purescript/purescript).
`zephyr` takes root terms, finds all terms which are required to evaluate
them, and generates code just for them.  This is done across all dependencies,
and can substantially reduce the size of PureScript bundles.  `zephyr` can also
evaluate some expressions (an experimental feature).

# Installation
The simplest option is to download the latest [release](https://github.com/coot/zephyr/releases) binary. You may also build from source (see "Build & Test" section below).

# Usage
```sh
# compile your project
purs compile -g corefn bower_components/purescript-*/src/**/*.purs src/**/*.purs

# run `zephyr`
zephyr -f Main.main
```
then you can bundle with `purs bundle` command:

```sh
purs bundle -o app.js -m Main dce-output/**/*.js
```
You can integrate it with other build tools, see
[below](#Integration-with-build-tools).

You can specify modules as entry points, which is the same as specifying all
exported identifiers.

```sh
# include all identifiers from Data.Eq module
zephyr Data.Eq

# as above
zephyr module:Data.Eq

# include Data.Eq.Eq identifier of Data.Eq module
zephyr ident:Data.Eq.Eq

# include 'Data.Eq.eq' identifier
zephyr Data.Eq.eq
```

`zephyr` reads corefn json representation from the `output` directory, removes
non transitive dependencies of entry points and generates common js modules (or
corefn representation) to `dce-output` directory.

# Evaluation of literal expressions

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
whenever `isProduction` is `true`.  This allows to have different
development and production environments while still ship a minified code which
only contains production code.  You may define `isProduction` in a module under
a `src-prod` directory and include it when compiling production code with `pulp
build -I src-prod` and to have another copy for your development environment
under `src-dev` where `isProduction` is set to `false`.

# Integration with build tools

`zephyr` can be integrated with

* [pulp](https://github.com/purescript-contrib/pulp): use
  `pulp build -- -g corefn` to compile, and `pulp browserify --skip-compile -o dce-output`
  to bundle `zephyr`'s output.
* [parcel](https://github.com/parcel-bundler/parcel)
* [spago](https://github.com/purescript/spago). See
  [this](https://github.com/thomashoneyman/purescript-halogen-realworld)
  example.  Use `spago build --purs-args '--codegen corefn,js'` to compile, using
  `spago bundle` is currently affected by
  [issue](https://github.com/purescript/spago/issues/216).

# Build & Test

```sh
cabal build exe:zephyr
```

To run tests

```sh
cabal run zephyr-test
```

# C Libraries

The released binaries are dynamically linked against `glibc`, so if your system is using `musl` (like Alpine Linux in `docker`) or another alternative C library, you will need to compile `zephyr` from source using `ghc` on that system.

# Comments

The `-f` switch is not 100% safe.  Upon running, `zephyr` will remove exports from
foreign modules that seems to be not used: are not used in PureScript code and
seem not to be used in the foreign module.  If you simply assign to `exports`
using JavaScript dot notation then you will be fine, but if you use square
notation `exports[var]` in a dynamic way (i.e. var is a true variable rather
than a string literal), then `zephyr` might remove code that shouldn't be
removed.
