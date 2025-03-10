# Version [0.1.5.2](https://github.com/sorki/haskell-zre/compare/0.1.5.1...0.1.5.2) (2024-03-10)

* Allow `network 3.2`

# Version [0.1.5.1](https://github.com/sorki/haskell-zre/compare/0.1.5.0...0.1.5.1) (2023-11-28)

* Fix missing `void` import for GHC9.6.3
* Allow `binary >= 0.8`

# Version [0.1.5.0](https://github.com/sorki/haskell-zre/compare/0.1.4.0...0.1.5.0) (2022-02-28)

* Use `attoparsec 0.14`

# Version [0.1.4.0](https://github.com/sorki/haskell-zre/compare/0.1.3.0...0.1.4.0) (2020-07-19)

* Changed `zfail` type from `String -> ZRE ()` to `String -> ZRE a`
* Handles errors in user application correctly instead of silently ignoring them
* Fixed incorrect time periods for pinger

# Version [0.1.3.0](https://github.com/sorki/haskell-zre/compare/0.1.2.0...0.1.3.0) (2020-07-14)

* Cabal fixes
  * Relaxed base bound
  * `cabal-version: 2.0`

# Version [0.1.2.0](https://github.com/sorki/haskell-zre/compare/0.1.1.0...0.1.2.0) (2020-07-10)

* Added
  * `zreChanWith` - principled variant of `zreChan` accepting runner function
  * `runZreEnvConfig` - runner utilizing config files only

# Version [0.1.1.0](https://github.com/sorki/haskell-zre/compare/0.1.0.2...0.1.1.0) (2020-06-17)

* Changelog started. Previous release was `0.1.0.2`.

* Breaking changes:
  * `zgossip_server` executable is renamed to `zgossip-server`
  * Removed `runZreOpts` in favor of `runZreParse` which now accepts another `optparse-applicative` parser
  * Groups now use `newtype Group` instead of plain String(s)
    and need to be constructed with `mkGroup`
  * `zrecvShouts` now requires `Group` and filters the message for this group only
  * Command line options changed:
    * `iface` renamed to `interface`
    * `mcast` renamed to `multicast-group`
    * Added `quiet-ping-rate`
  * No longer ships with `stack.yaml`

* Additions
  * Experimental `Network.ZRE.Chan` for working with typed channels

* Minor:
  * Spots new `examples` flag for building with examples (disabled by default)

---

`zre` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org

