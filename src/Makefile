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

test1-success: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test1-success

test1-fail: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test1-fail

test1: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test1

test2: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test2

test3a-success: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test3a-success

test3a-fail: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test3a-fail

test3a: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test3a

test3: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test3

typecheck-success: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test typecheck-success

typecheck-fail: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test typecheck-fail

typecheck: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test typecheck

optimize: all
	@JOUJOU="../src/joujou.exe" $(MAKE) -C ../test optimize
