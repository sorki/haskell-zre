let
  lvirt = {
    deployment.targetEnv = "libvirtd";
    deployment.libvirtd.headless = true;
    deployment.libvirtd.extraDevicesXML = ''
      <serial type='pty'>
        <target port='0'/>
      </serial>
      <console type='pty'>
        <target type='serial' port='0'/>
      </console>
    '';
  };
in
{
  zre1 = lvirt;
  zre2 = lvirt;
  zgossip = lvirt;
}
