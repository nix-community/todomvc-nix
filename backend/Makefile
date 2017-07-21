UNAME="$(shell uname -s)"

all: scotty.docker

%.bin:
ifeq ($(UNAME), "Darwin")
	docker run --rm -it -v ~/.stack:/root/.stack -v `pwd`:/code -w /code fpco/stack-build bash -c 'stack build todobackend-$(basename $@) --allow-different-user'
else
	stack build todobackend-$(basename $@)
endif

%.docker: %.bin
	docker build -f docker/Dockerfile-$(basename $@) -t jhedev/todobackend-haskell:$(basename $@) .

push:
	docker push jhedev/todobackend-haskell
