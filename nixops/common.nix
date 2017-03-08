{ config, pkgs, lib, ... }:
{
  config = {
    boot.kernelParams = [ "console=ttyS0,115200" ];
    services.openssh.enable = true;
    services.openssh.permitRootLogin = "yes";
    #users.extraUsers.root.openssh.authorizedKeys.keys = [ "" ];
  };
}


