{ config, pkgs, lib, ... }:
let
  zre = import ../default.nix {};
in
{
  config =
    {
      networking.hostName = "zre";
      environment.systemPackages = [ zre ];

      networking.firewall.allowedTCPPortRanges = [ { from = 41000; to = 41100; } ];
      networking.firewall.allowedUDPPorts = [ 5670 ];

      systemd.services.zre =
        { description = "ZRE";
          path = [ pkgs.iproute ];
          wantedBy = [ "multi-user.target" ]; # Specify that the system wants this service to run.
          after = [ "network.target" ]; # Start the webserver after the network has come up.
          serviceConfig =
            { ExecStart = "${zre}/bin/zretime"; # Give the absolute path of zre to systemd.
            };
        };
    };
}

