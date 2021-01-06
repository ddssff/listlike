[![Build Status](https://secure.travis-ci.org/ddssff/ListLike.png?branch=master)](http://travis-ci.org/ddssff/ListLike)
[![Build Status](https://github.com/ddssff/ListLike/workflows/Haskell-CI/badge.svg)](https://github.com/ddssff/ListLike/actions)

ListLike
========

The `ListLike` package provides typeclasses and instances to allow
polymorphism over many common datatypes.

CHANGES
=======

* 4.7.4: support utf8-string-1.0.2 ([#10](https://github.com/ddssff/listlike/issues/10))

* 4.7.3 (2020-12-31):
  - support bytestring-0.10.12 ([#7](https://github.com/ddssff/listlike/pull/7)) for ghc 8.10.3
  - removed support for ghc 7.6 and 7.8 ([#6](https://github.com/ddssff/listlike/issues/6))

* 4.7.2 (2020-08-21): support dlist-1.0 ([#4](https://github.com/ddssff/listlike/issues/4))

* 4.7.1 (2020-07-03): support QuickCheck-2.14

Version 4.7
-----------

  * Make `GHC.Exts.IsList` a superclass of `ListLike` and use its `fromList` and `toList` methods
  * make `Data.String.IsString` a superclass of `Stringlike` and use its `fromString` method
  * Add methods to `StringLike`: `show`, `fromText`, `fromLazyText`
  * Add a class `ListOps`, alternative to `ListLike`, that uses the `GHC.Exts.Item` instead of
    the `item` type parameter.
  * Supply `instance IsString Seq` for old versions of container
