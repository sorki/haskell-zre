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

