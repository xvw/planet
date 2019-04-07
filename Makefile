.PHONY: all build clean repl doc fmt install test dev binaries

# Developement's workflow

all: build binaries

build:
	dune build @install

clean:
	dune clean
	rm *.exe

repl: all
	dune utop

doc:
	dune build @doc

fmt:
	dune build @fmt --auto-promote

test:
	dune runtest -f

# Build binaries
%.exe : build
	dune build src/bin/$@
	cp _build/default/src/bin/$@ ./

binaries: project/project.exe log/log.exe build/build.exe

# Package installation

install:
	opam install -y .
	eval $(opam env)
	dune install
	dune build @install

dev: install

# Initialize developement environement

dev-deps:
	opam install -y dune
	opam install -y alcotest
	opam install -y merlin
	opam install -y ocp-indent
	opam install -y ocamlformat
	opam install -y utop
	opam install -y odoc
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

up:
	opam update
	opam upgrade
	eval $(opam env)
