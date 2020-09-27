# todobackend-haskell

[![build status](https://circleci.com/gh/jhedev/todobackend-haskell.svg?style=shield)](https://circleci.com/gh/jhedev/todobackend-haskell/tree/master)

This repository provides different Haskell implementations for [todobackend](http://www.todobackend.com/).

The [`todobackend-common`](https://github.com/jhedev/todobackend-haskell/tree/master/todobackend-common) package
implements common functionality, such as the model and some utils.

### Building and running locally

Make sure you have [`stack`](https://github.com/commercialhaskell/stack) installed.

To build and run `todobackend-scotty` execute the following:

```
stack build todobackend-scotty # This will take some time
PORT=3000 URL=http://localhost:3000 stack exec todobackend-scotty
```

### Docker images

To run the docker container of the scotty implementation:

```
docker run --rm -it -p 3000:3000 -e URL=http://localhost:3000 jhedev/todobackend-haskell:scotty
```

The application is now running on port 3000.
