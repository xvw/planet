# Developement's workflow

all: build

build:
	dune build @install

clean:
	dune clean
	rm -R bin

repl: all
	dune utop

doc:
	dune build @doc

fmt:
	dune build @fmt --auto-promote

# Build binaries
%.exe : build
	mkdir -p ./bin
	dune build src/bin/$@
	cp _build/default/src/bin/$@ ./bin

# Package installation

install:
	opam install -y .
	dune install
	dune build @install

# Initialize developement environement

dev-deps: 
	opam install -y merlin
	opam install -y ocp-indent
	opam install -y ocamlformat
	opam install -y utop
	opam install user-setup
	opam user-setup install

switch:
	opam switch -y create .
	eval $(opam env)

local-install: switch
	opam install -y .

setup-dev:
	opam switch -y create .
	eval $(opam env)
	opam install -y .

remove-switch:
	opam switch -y remove .
