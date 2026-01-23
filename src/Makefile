.PHONY: all clean format build test

all: format build

clean:
	@dune clean

format:
	@dune fmt

build:
	@dune build

test: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test

test1: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test1

test2: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test2

test3: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test3

typecheck: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test typecheck

optimize: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test optimize
