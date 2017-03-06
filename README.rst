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

Examples
--------

Few sample applications are provided to get you started:

 - zre - interact and dump events
 - zrecat <group> - cat messages for group
 - zretime - send time to time group

These can be installed locally with `stack install` command.
