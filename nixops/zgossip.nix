{ config, pkgs, lib, ... }:
let
  zre = import ../default.nix {};
in
{
  config =
    {
      networking.hostName = "zgossip";
      environment.systemPackages = [ zre ];

      networking.firewall.allowedTCPPorts = [ 31337 ];

      systemd.services.zgossip =
        { description = "ZGossip server";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig =
            { ExecStart = "${zre}/bin/zgossip_server";
            };
        };
    };
}
