#!/usr/bin/env bash
# Run nix under docker
set -euo pipefail

show_usage() {
  local exe=$(basename "$0")
  cat <<USAGE
Usage:
  $exe [shell]             - *enters the nix-shell
  $exe run <cmd> [args...] - *runs a command inside of the container
  $exe start               - starts the container
  $exe stop                - stops the container
  $exe rm [-f]             - deletes the container
  $exe logs [-f]           - show the container logs
  $exe -h | --help         - show this help

* these commands also automatically start the container
USAGE
}

## Main ##

cd "$(dirname "$0")"
container_name=$(basename "$(pwd)")
container_options=(
  --name "$container_name"
  -v $PWD:/src
  --workdir /src
)
start_container() {
  if ! docker inspect "$container_name" &>/dev/null; then
    docker create "${container_options[@]}" nixos/nix /bin/sh -c "nix-env -f https://nixos.org/channels/nixos-17.09/nixexprs.tar.xz -iA nixUnstable bashInteractive cacert && exec nix-daemon"
  fi
  docker start "$container_name"
}

cmd=${1:-}
if [[ -z "$cmd" ]]; then
  cmd=shell
fi
shift || true

case "$cmd" in
"shell")
  start_container
  docker exec -ti -e NIX_REMOTE=daemon "$container_name" nix-shell "$@"
  ;;
"run")
  start_container
  docker exec -ti -e NIX_REMOTE=daemon "$container_name" "$@"
  ;;
"start")
  start_container
  ;;
"stop")
  docker stop "$@" "$container_name"
  ;;
"rm")
  docker rm "$@" "$container_name"
  ;;
"logs")
  docker logs "$@" "$container_name"
  ;;
*)
  show_usage
  ;;
esac
