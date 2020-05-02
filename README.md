# extensions

[![GitHub CI](https://github.com/kowainik/extensions/workflows/CI/badge.svg)](https://github.com/kowainik/extensions/actions)
[![Hackage](https://img.shields.io/hackage/v/extensions.svg?logo=haskell)](https://hackage.haskell.org/package/extensions)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

Library and CLI tool to parse Haskell `{-# LANGUAGE #-}` extensions in
source files, extract `default-extensions` from `.cabal` files and
combine together `default-extensions` and per-module extensions.

## Goals

The `extensions` library provides a lighweight way to get Haskell
LANGUAGE pragmas for Haskell modules. It has the following goals:

1. Be **lightweight**. Dependency footprint is extremely small,
   and using `extensions` either as a library or as a tool is
   straightforward.
2. Support both `default-extensions` in **Cabal** and `{-# LANGUAGE #-}`
   pragmas in **Haskell** modules.
3. Should work on common and real cases. `extensions` strives to
   support as many valid syntactic constructions as possible, but it
   may not work on every possible combination of CPP, comments and
   pragmas, where GHC would work. We encouragle you to
   [open issue](https://github.com/kowainik/extensions/issues/new) if
   you encounter any failures when using `extensions`.

## How to use

You can use `extensions` either as a library or as a CLI tool.

### Library

#### Usage with Cabal

`extensions` is compatible with the latest GHC compiler
versions starting from `8.8.3`.

In order to start using `extensions` in your project, you
will need to set it up with the three easy steps:

1. Add the dependency on `extensions` in your project's
   `.cabal` file. For this, you should modify the `build-depends`
   section by adding the name of this library. After the adjustment,
   this section could look like this:

   ```haskell
   build-depends: base ^>= 4.14
                , extensions ^>= 0.0
   ```
2. In the module where you wish to extract extensions, you
   should add the import:

   ```haskell
   import Extensions (getPackageExtensions)
   ```
3. Now you can use the types and functions from the library:

   ```haskell
   main :: IO ()
   main = getPackageExtensions "extensions.cabal" >>= print
   ```

#### Usage with Stack

If `extensions` is not available on your current Stackage
resolver yet, fear not! You can still use it from Hackage by adding
the following to the `extra-deps` section of your `stack.yaml` file:

```yaml
extra-deps:
  - extensions-0.0.0.0
```

### CLI tool

To use `extensions` as a CLI tool, you need to install it either with Cabal

```
cabal install extensions
```

or Stack

```
stack install extensions
```

The tool can be used to inspect language extensions in your Haskell
project. Some common usages of the tool:

1. Get all extensions in all modules, combined with
   `default-extensions` from the `.cabal` file.
   ![All extensions](https://user-images.githubusercontent.com/4276606/80870176-b0f73800-8c9c-11ea-8ffc-dda2d4940d1e.png)
2. Get all extensions for a specific module, combined with Cabal
   extensions.
   ![Module extensions](https://user-images.githubusercontent.com/4276606/80870175-b05ea180-8c9c-11ea-9f48-cac7ff854b9c.png)
3. Get extensions defined only in a module.
   ![Only module](https://user-images.githubusercontent.com/4276606/80870173-afc60b00-8c9c-11ea-9d74-cf92ed0c3940.png)

## Alternatives

Alternatively, you can extract Haskell Language extensions using the
following ways:

1. Using `ghc` as a library. This approach ties you to a specific GHC
   version and requires more effort to support multiple GHCs. Also,
   GHC API is more complicated than `extensions` API (especially if
   you want to handle `CPP`). And even with `CPP` handling from `ghc`
   you won't be able to get all extensions defined in a
   module. However, this approach allows you to fully reproduce GHC
   behaviour.
2. Using `ghc-lib-parser`. Same as the previous approach, but does not
   tie you to a specific GHC version. However, `ghc-lib-parser` is
   rather big dependency.
3. Using the `haskell-src-exts` library. This library parses Haskell
   source files, but it's not actively maintained anymore and doesn't
   support `CPP`.
