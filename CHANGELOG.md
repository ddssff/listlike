CHANGES
=======

### 4.7.7 (2022-05-26)

  - methods `sequence` and `mapM`: relax `Monad` constraint to `Applicative`
  - `LANGUAGE TypeOperators` to fix GHC 9.4 warning
  - allow `text-2.0`
  - tested with GHC 7.10 - 9.4

### 4.7.6 (2021-09-01)

  - new implementation of `tail` in `DList` instance
  - warning-free for `-Wall` and `-Wcompat`

### 4.7.4 (2021-01-07)

  - support utf8-string-1.0.2 ([#10](https://github.com/ddssff/listlike/issues/10))

### 4.7.3 (2020-12-31)

  - support bytestring-0.10.12 ([#7](https://github.com/ddssff/listlike/pull/7)) for ghc 8.10.3
  - removed support for ghc 7.6 and 7.8 ([#6](https://github.com/ddssff/listlike/issues/6))

### 4.7.2 (2020-08-21)

  - support dlist-1.0 ([#4](https://github.com/ddssff/listlike/issues/4))

### 4.7.1 (2020-07-03)

  - support QuickCheck-2.14

## 4.7

  - make `GHC.Exts.IsList` a superclass of `ListLike` and use its `fromList` and `toList` methods
  - make `Data.String.IsString` a superclass of `Stringlike` and use its `fromString` method
  - add methods to `StringLike`: `show`, `fromText`, `fromLazyText`
  - add a class `ListOps`, alternative to `ListLike`, that uses the `GHC.Exts.Item` instead of
    the `item` type parameter
  - supply `instance IsString Seq` for old versions of container
