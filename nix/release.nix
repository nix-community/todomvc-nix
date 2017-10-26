/**
 * This files contains all the derivations that we want to build
 * as a channel.
 */
let pkgs = import ./.; in {

  inherit (pkgs)
    frontend
    frontend-docker
    backend
    backend-docker
    scripts
    ;
}
