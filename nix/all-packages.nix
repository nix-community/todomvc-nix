/**
 * This is an overlay with all the packages and overrides for this repo.
 */
self: super:

with super;

{
  yarn2nix = self.callPackage ./yarn2nix {};

  inherit (self.yarn2nix) mkYarnPackage;

  frontend = self.callPackage ./frontend {};

  frontend-image = dockerTools.buildImage {
    name = "todo-frontend";
    contents = [ self.frontend self.caddy ];
    config = {
      Cmd = [ "/bin/caddy" ];
      WorkingDir = "/var/www";
      ExposedPorts = {
        "2115/tcp" = {};
      };
      Env = [
        "PATH=/bin"
      ];
    };
  };


  haskellPackages = self.callPackage ./backend {};

  backend = self.haskellPackages.todobackend-scotty;

  backend-image = dockerTools.buildImage {
    name = "todo-backend";
    contents = [ self.backend ];
    config = {
      Cmd = [ "/bin/todobackend-scotty" ];
      WorkingDir = "/data";
      Volumes = {
        "/data" = {};
      };
      ExposedPorts = {
        "3000/tcp" = {};
      };
      Env = [
        "PORT=3000"
        "URL=http://localhost:3000"
        "PATH=/bin"
      ];
    };
  };

}
