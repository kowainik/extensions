# Changelog

`extensions` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.1.0.1 — Oct 15, 2023

* Add support for GHC-9.6

* Add support for `Cabal` `3.10` and remove support for all other
  `Cabal` versions.  This seems like the sensible thing to do, because
  the list of extensions in the `Cabal` package changes every major
  version.  If this causes you problems please [file an
  issue](https://github.com/kowainik/extensions/issues/new) and we
  will address it.

## 0.1.0.0 — Oct 14, 2022

* [#74](https://github.com/kowainik/extensions/issues/74):
  Support GHC-9.2.
* [#83](https://github.com/kowainik/extensions/issues/83):
  Support GHC-9.4.
* Support `Cabal` `3.4`, `3.6` and `3.8`.
* [#70](https://github.com/kowainik/extensions/issues/70):
  Parse empty lines and spaces in before comments and pragmas in the beginning
  of the file.

## 0.0.0.1 — May 9, 2020 🎖️

* Handle one more parsing case.

## 0.0.0.0 — May the 4th, 2020 💪

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/extensions/releases
