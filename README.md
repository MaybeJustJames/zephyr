# zephyr
[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)

Experimental tree shaking tool for [PureScript](https://github.com/purescript/purescript).

# Usage
```
# compile your project (or use `pulp build --dump-corefn`)
pure compile --dump-corefn bower_components/purescript-*/src/**/*.purs src/**/*.purs

# run `zephyr`
zephyr -O1 Main.main

# bundle your code
webpack
```

`zephyr` reads corefn json representation from `output` directory, removes non
transitive dependencies of entry points and dumps common js modules (or corefn
representation) to `dce-output` directory.

# Comments

The `-O1` switch is not 100% safe.  When on `zephyr` will remove exports from
foreign modules that seems to be not used: are not used in purescript code and
seem not to be used in the foreign module.  If you simply assign to `exports`
using javascript dot notation then you will be fine, but if you use square
notation `exports[var]` in a dynamic way (i.e. var is a true variable rather
than a string literal) then `zephyr` might remove code that shouldn't be
removed.

It is good to run `webpack` or `rollup` to run a javascript tree shaking
algorithm on the javascript code that is pulled in your bundle by your by your
foreign imports.

# Tested

It is tested on dozeon of various projects of
[@alexmingoia](https://github.com/alexmingoia),
[@bodil](https://github.com/bodil), [@coot](https://github.com/coot),
[purescript-contrib](https://github.com/purescript-contrib) and
[slamdata](https://github.com/slamdata) including:
* [purescript-pux](https://github.com/alexmingoia/purescript-pux)
* [purescript-smolder](https://github.com/bodil/purescript-smolder)
* [purescript-signal](https://github.com/bodil/purescript-signal)
* [purescript-aff](https://github.com/slamdata/purescript-aff)
* [purescript-matryoshka](https://github.com/slamdata/purescript-matryoshka)
* [purescript-argonaout](https://github.com/purescript-contrib/purescript-argonaut)
* [purescript-profunctor-lenses](https://github.com/purescript-contrib/purescript-profunctor-lenses)
* [purescript-react-hocs](https://github.com/coot/purescript-react-hocs) (_karma tests_)
* [purescript-react-redox](https://github.com/coot/purescript-react-redox) (_karma test_)

Checkout unit test suite.
