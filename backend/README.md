# todobackend-haskell

[![build status](https://circleci.com/gh/jhedev/todobackend-haskell.svg?style=shield)](https://circleci.com/gh/jhedev/todobackend-haskell/tree/master)

This repository provides different Haskell implementations for [todobackend](http://www.todobackend.com/).

The [`todobackend-common`](https://github.com/jhedev/todobackend-haskell/tree/master/todobackend-common) package
implements common functionality, such as the model and some utils.

### Demos

Hosting sponsored by **[sloppy.io](https://sloppy.io)**.

The running demos can be found at:

* [todobackend-happstack.sloppy.zone](https://todobackend-happstack.sloppy.zone)
* [todobackend-scotty.sloppy.zone](https://todobackend-scotty.sloppy.zone)
* [todobackend-servant.sloppy.zone](https://todobackend-servant.sloppy.zone)
* [todobackend-snap.sloppy.zone](https://todobackend-snap.sloppy.zone)
* [todobackend-spock.sloppy.zone](https://todobackend-spock.sloppy.zone)
* [todobackend-yesod.sloppy.zone](https://todobackend-yesod.sloppy.zone)

### Building and running locally

Make sure you have [`stack`](https://github.com/commercialhaskell/stack) installed.

To build and run `todobackend-scotty` execute the following:

```
stack build todobackend-scotty # This will take some time
PORT=3000 URL=http://localhost:3000 stack exec todobackend-scotty
```

Similar for the other implementations.

### Docker images

To run the docker container of the scotty implementation:

```
docker run --rm -it -p 3000:3000 -e URL=http://localhost:3000 jhedev/todobackend-haskell:scotty
```

The application is now running on port 3000. For any other implementation just replace `scotty` with `servant`, `snap`, `spock` or `yesod`.
