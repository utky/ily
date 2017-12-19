.PHONY: all build test 

all: build test

build:
	stack build

test:
	stack test
