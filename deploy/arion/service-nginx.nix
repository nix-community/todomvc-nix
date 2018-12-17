/*

  DISCLAIMER

  This uses a somewhat hidden feature in NixOS, which is the
  "runner". It's a script that's available on systemd services
  that lets you run the service independently from systemd.
  However, it was clearly not intended for public consumption
  so please use it with care.
  It does not support all features of systemd so you are on
  your own if you use it in production.

 */

{ config, pkgs, lib, ... }:

let
  makeVirtual = {
    config.boot.loader.grub.enable = false;
    config.fileSystems."/".device = "/dev/null";
  };

  inherit (webNixOS) run-nginx;

  webNixOS = pkgs.nixos ({ lib, pkgs, config, ... }: {
    imports = [makeVirtual];
    config.system.build.run-nginx = pkgs.writeScript "run-nginx" ''
      #!${pkgs.bash}/bin/bash
      PATH='${config.systemd.services.nginx.environment.PATH}'
      echo nginx:x:${toString config.users.users.nginx.uid}:${toString config.users.groups.nginx.gid}:nginx web server user:/var/empty:/bin/sh >>/etc/passwd
      echo nginx:x:${toString config.users.groups.nginx.gid}:nginx >>/etc/group
      ${config.systemd.services.nginx.runner}
    '';

    config.services.nginx = nginxConfig;

  });

  nginxConfig = lib.mkMerge config.nginx.config;

in
{
  options = {
    nginx.config = lib.mkOption {
      default = {};
      type = with lib.types; coercedTo attrs (a: [a]) (listOf attrs);
    };
  };

  config = {
    service.command = [ run-nginx ];
    nginx.config.enable = true;
  };
}
