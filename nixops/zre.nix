let
  zre = { config, pkgs, lib, ... }: {
  imports =
    [
      ../zretest.nix
      ./common.nix
    ];
  };
  zgossip = { config, pkgs, lib, ... }: {
  imports =
    [
      ../zgossip.nix
      ./common.nix
    ];
  };
in
{
  network.description = "ZRE network";

  zre1 = zre;
  zre2 = zre;
  zgossip = zgossip;
}
