ZRE protocol implementation https://rfc.zeromq.org/spec:36/ZRE/

Work in progress, most protocol / networking stuff is working,
interface needs some work.

Usage:

stack build
stack exec zre
# in another terminal
stack exec zre


Two zre peers should find each other and be able to send message between each other.
Make sure firewall is disabled as it needs arbitrary ports for now.
