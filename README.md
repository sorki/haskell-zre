# zre

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/sorki/haskell-zre/ci.yaml?branch=master)](https://github.com/sorki/haskell-zre/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/zre.svg?color=success)](https://hackage.haskell.org/package/zre)
[![Dependencies](https://img.shields.io/hackage-deps/v/zre?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=zre)

ZRE protocol implementation https://rfc.zeromq.org/spec:36/ZRE/

Peer-to-peer local area networking with reliable group messaging
and automatic peer discovery.

## Usage

Dependencies:

```
zeromq4
```

Clone and test

```sh
git clone https://github.com/sorki/haskell-zre/
cd haskell-zre
nix-build
./result/bin/zre
# in another terminal or networked computer
./result/bin/zre
```

Two zre peers should find each other and be able to send message between each other.
Firewall needs to allow traffic on UDP port 5670 and TCP port range 41000-41100.
Application picks random port from this range and advertises it to network.

## Applications

Few applications are provided to get you started:

 - zre - interact and dump events
 - zrecat <group> - cat messages for group

These can be installed locally using `pkgs.haskellPackage.zre`.

Try running multiple copies of `zre` and `zrecat` on
the same computer or computers on the local network

```sh
zre
# another terminal
zrecat test
# now in original terminal you can join testgroup with
> /join test
# or send messages to it
> /shout test msg
```

Send uptime periodically to uptime group

```sh
( while true; do uptime; sleep 1; done ) | zrecat uptime
```

Cat file to group

```sh
cat /etc/os-release | zrecat test
```

Interact manually

```sh
zre
# in zre shell following commands are supported:
> /join time
> /shout time test!
> /leave time
> /join uptime
> /whisper <uuid> message
```

## ZGossip

Implementation of gossip protocol is included in form of key value TTL server.
This allows connecting peers from different networks (or subnets) not reachable via multicast
beacon. This service requires TCP port 31337 and can be started with `zgossip-server` binary.

Run server

```sh
zgossip_server
```

Pass gossip endpoint to apps with

```sh
zre -g <gossip_ip>:31337
```

## Configuration

ZRE applications using `runZre` will automatically try to load configuration
file if `ZRECFG` environment variable points to it. See `zre.conf` for configuration
example::

```sh
ZRECFG=./zre.conf zrecat test
```

To be able to use one config for multiple apps and still be able to distinguish between
them you can also set `ZRENAME` environment variable which overrides name
from config or default config if `ZRECFG` is not used::

```sh
ZRENAME=zrenode1 zrecat test
```

## Demos

* https://asciinema.org/a/106340
