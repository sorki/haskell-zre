nixops deployment example
=========================


Deploys zre and zgossip_server nodes to libvirt virtual machines.

Usage::

        # adjust authorized_keys in common.nix
        nixops create ./zre.nix ./zre-libvirt.nix -d zre
        nixops deploy
