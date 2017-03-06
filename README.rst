zre
===

ZRE protocol implementation https://rfc.zeromq.org/spec:36/ZRE/

Peer-to-peer local area networking with reliable group messaging
and automatic peer discovery.

Usage
-----

Clone and test::

        git clone https://git.48.io/zre/
        cd zre
        stack build
        stack exec zre
        # in another terminal
        stack exec zre


Two zre peers should find each other and be able to send message between each other.
Firewall needs to allow traffic on UDP port 5670 and TCP port range 41000-41100.
Application picks random port from this range and advertises it to network.

Examples
--------

Few sample applications are provided to get you started:

 - zre - interact and dump events
 - zrecat <group> - cat messages for group
 - zretime - send time to time group

These can be installed locally with `stack install` command.

ZGossip
-------

Implementation of gossip protocol is included in form of key value ttl server.
This allows connecting peers from different networks (or subnets) not reachable via multicast
beacon. This service requires TCP port 31337 and can be started with `zgossip_server` binary.

FIXME: zgossip server IP is hardcoded, configurator needed /o\o/
