.PHONY: all build clean repl doc fmt install test dev binaries

# Developement's workflow

all: build binaries build-hakyll build-pages

build:
	dune build @install

clean: clean-pages
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
	opam install . --deps-only --yes
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



up:
	opam update
	opam upgrade

setup-hakyll:
	asdf install
	stack install hakyll --resolver lts-12.26

init-hakyll:
	stack init --resolver lts-12.26

build-hakyll:
	stack build

build-pages:
	stack exec site build

rebuild-pages:
	stack exec site rebuild

watch-pages:
	stack exec site watch

clean-pages:
	stack exec site clean

web-reset: clean
	stack build
	stack exec site rebuild
