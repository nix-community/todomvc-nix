# TodoMVC w/ Haskell backend and JavaScript frontend

This project build can be demonstrates how a project can be nixified. The service itself has been cobbled-up together using the https://www.todobackend.com/ and isn't the interesting bit.

What's interesting is how to put together the nix/ folder, look at the git commit history it should be fairly logical.

## Usage

Here are the various usage patterns:

* `./script/ci` would be invoked by the CI (eg: Jenkins), it builds, runs the tests and pushes to the binary cache.
* `./script/run` builds and runs the service locally
* invoke `nix-shell` in each service to import development dependencies

## TODO

I didn't have the time to implement everything. Here are the missing bits:

* `./script/ci` should also push the docker images to the registry
* setup the binary cache
* handle docker images on Darwin
* improve the `nix-shell` experience
* pin `<nixpkgs>`

Let me know if you want to see any of these happening.
