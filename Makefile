.PHONY: build test hdevstart hdevstop

build:
	stack build

test:
	stack test

hdevstart:
	hdevtools admin --start-server

hdevstop:
	hdevtools admin --stop-server
