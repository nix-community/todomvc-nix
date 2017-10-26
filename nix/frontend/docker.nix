{ dockerTools, frontend, caddy }:
dockerTools.buildImage {
  name = "todo-frontend";
  contents = [ frontend caddy ];
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
}
