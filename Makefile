root     := $(shell stack path --local-install-root)
exe      := "${root}/bin/hauth-exe"
version  := $(shell stack query locals hauth version | tr -d "'")

default: build

dependancies:
	dnf install -y postgresql-devel

development-tools:
	stack setup
	stack install ghc-mod
	stack install stylish-haskell
	stack install hscolour

build:
	stack build --no-copy-bins

test:
	stack test

install: build
	cp ${exe} /usr/local/bin/

clean:
	stack clean

container: build
	docker build --build-arg exe=$(shell realpath --relative-to="." "${exe}") -t b0d0nne11/hauth:latest -t b0d0nne11/hauth:${version} .

haddock:
	stack haddock
	@echo "" # newline
	@echo "View the module documentation at file://${root}/doc/index.html"

swagger: container
	-docker-compose stop app
	-docker-compose rm -f app
	docker-compose up -d
	@echo "" # newline
	@echo "View the HTTP API specification at http://localhost:8081/"

key:
	tr -dc _A-Z-a-z-0-9 </dev/urandom | head -c64 | base64 -w0
