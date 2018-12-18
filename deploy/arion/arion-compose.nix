{ pkgs, lib, config, uid, ... }:

let
  serveStorePath = pkg:
    let
      etag = builtins.substring 11 32 "${pkg}";
    in
      {
        root = pkg;
        extraConfig = ''
          etag off;
          add_header etag "${etag}";
          index index.html;
        '';
      };
  apiInternalURL = "http://backend:8080";
  # Disambiguate config at the environment level from
  # config at the service level
  envConfig = config;

in
{
  options.domain = lib.mkOption {
    type = lib.types.string;
    default = "localhost:8888";
  };

  config.docker-compose.services = {

    backend = {
      service.useHostStore = true;
      # For example:
      # service.depends_on = [ "postgres" ];
      service.command = [ "sh" "-c" ''
                  ${pkgs.coreutils}/bin/ln -sf ${pkgs.iana-etc}/etc/protocols /etc/protocols
                  ${pkgs.coreutils}/bin/ln -sf ${pkgs.iana-etc}/etc/services /etc/services
                  ${pkgs.coreutils}/bin/mkdir -p /etc/ssl/certs/
                  ${pkgs.coreutils}/bin/ln -sf ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-bundle.crt
                  ${pkgs.coreutils}/bin/ln -sf ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt
                  cd /work
                  ${pkgs.gosu}/bin/gosu ${uid} ${pkgs.backend}/bin/todobackend-scotty
                '' ];
      service.environment.PORT = "8080";
      service.environment.URL = "http://${envConfig.domain}";
      # For example to mount a directory with 'secrets':
      service.volumes = [ "${toString ../..}:/work" ];
    };

    web = {
      imports = [ ./service-nginx.nix ];
      service.useHostStore = true;
      service.depends_on = [ "backend" ];
      service.ports = [
        "8888:80" # host:container
      ];
      nginx.config = {
        recommendedOptimisation = true;
        appendHttpConfig = ''
          server_names_hash_bucket_size 64;
          etag off;
        '';
        virtualHosts."${envConfig.domain}" = {
          # forceSSL = true;
          # enableACME = true;
          locations."/todos" = {
            proxyPass = apiInternalURL;
            proxyWebsockets = false;
            extraConfig = ''
              proxy_set_header X-Forwarded-For $remote_addr;
            '';
          };
          locations."/" = serveStorePath "${pkgs.frontend}/var/www";
        };
      };
    };
  };

}
