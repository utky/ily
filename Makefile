.PHONY: all build test hdevstart hdevstop

all: build test

build:
	stack build

test:
	stack test

happy:
	stack exec happy -- -i src/Ily/Parser.y

hdevstart:
	hdevtools admin --start-server

hdevstop:
	hdevtools admin --stop-server
