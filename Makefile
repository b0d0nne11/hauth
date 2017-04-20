build_path   := $(shell stack path --local-install-root)/bin/hauth-exe
install_path := $(shell stack path --local-bin)/hauth-exe
doc_path     := $(shell stack path --local-doc-root)/index.html
project_root := $(shell stack path --project-root)
version      := $(shell stack query locals hauth version | tr -d "'")

build:
	stack build --haddock --no-copy-bins

test:
	stack test

install: build
	cp $(build_path) $(dir install_path)

clean:
	rm $(install_path)
	stack clean

dev-tools:
	stack setup
	stack install ghc-mod
	stack install stylish-haskell
	stack install hscolour

container: build
	docker build --build-arg exe=$(shell realpath --relative-to="." "$(build_path)") -t b0d0nne11/hauth:latest -t b0d0nne11/hauth:v$(version) .

swagger-ui:
	-docker stop swagger-ui
	-docker rm swagger-ui
	docker run -d --name swagger-ui \
		-e API_URL=http://localhost:8081/hauth.yaml \
		-v $(project_root)/swagger.yaml:/usr/share/nginx/html/hauth.yaml \
		-p 8081:8080 \
		swaggerapi/swagger-ui

docs: build swagger-ui
	@echo "" # newline
	@echo "Documentation links:"
	@echo "  Haddock - file://$(doc_path)"
	@echo "  Swagger - http://localhost:8081/"

key:
	tr -dc _A-Z-a-z-0-9 </dev/urandom | head -c64 | base64 -w0

.PHONY: build test install clean dev-tools container swagger-ui docs key
