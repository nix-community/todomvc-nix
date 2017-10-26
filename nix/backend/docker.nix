{ dockerTools, backend }:
dockerTools.buildImage {
  name = "todo-backend";
  contents = [ backend ];
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
}
