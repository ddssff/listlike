[![Build Status](https://secure.travis-ci.org/ddssff/ListLike.png?branch=master)](http://travis-ci.org/ddssff/ListLike)

ListLike
========

The `ListLike` package provides typeclasses and instances to allow
polymorphism over many common datatypes.

CHANGES
=======
Version 4.7
-----------

  * Make `GHC.Exts.IsList` a superclass of `ListLike` and use its `fromList and `toList` methods
  * make `Data.String.IsString` a superclass of `Stringlike and use its `fromString` method
  * Add methods to `StringLike`: `show`, `fromText`, `fromLazyText`
  * Add a class `ListOps`, alternative to `ListLike`, that uses the `GHC.Exts.Item` instead of
    the `item` type parameter.
  * Supply `instance IsString Seq` for old versions of container
