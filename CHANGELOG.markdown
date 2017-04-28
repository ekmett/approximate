next
----
* Replace use of `Hashable1` from `hashable-extras` in favor of `Hashable` from
  `hashable-1.2.5.0`. As a result, the `hashable-extras` dependency has been removed.
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

0.2.2.3
-------
* Added support for `safecopy` 0.9 and `cereal` 0.5

0.2.2.1
-------
* Compiles warning-free on GHC 7.10

0.2.1.1
-------
* Bumped `cereal` bounds

0.2
---
* Removed the `data-default` dependency
* Increased range of supported versions of `generic-deriving`.

0.1.1
-----
* Ported `Data.Approximate.Numerics` from [analytics](http://github.com/analytics)

0.1
---
* Ported `Data.Approximate.Type` and `Data.Approximate.Mass` from [analytics](http://github.com/analytics)
