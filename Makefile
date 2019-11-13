SHA=`eval "git rev-parse HEAD"`
MSG="[Automatic] $(SHA)"

.PHONY: all build clean repl doc fmt install test dev binaries

# Developement's workflow

all: render

init_submodule:
	git submodule add git@github.com:xvw/xvw.github.io.git deployement

build:
	dune build @install

clean: clean-pages
	dune clean
	rm *.exe

repl: build
	dune utop

doc:
	dune build @doc

fmt:
	dune build @fmt --auto-promote

test:
	dune runtest -f

# Build binaries
%.exe: build
	dune build src/bin/$@
	cp _build/default/src/bin/$@ ./

%.bc.js: build
	mkdir -p _seeds
	dune build src/$@ --profile release
	cp _build/default/src/$@ ./_seeds

binaries: project/project.exe log/log.exe build/build.exe now/now.exe

client: facade/facade.bc.js

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
	rm -rf _seeds
	stack exec site clean

web-reset: clean
	(make build; make binaries; ./build.exe all)
	stack build
	stack exec site rebuild

render: binaries client
	./build.exe all
	stack build
	stack exec site rebuild

hydrate: binaries
	./build.exe all

rehydrate: binaries client
	./build.exe all

s:
	./build.exe all
	stack exec site build

prepublish: render

publish: prepublish
	git submodule update --remote --merge
	rsync -avr --delete --exclude-from '.publishignore'  _site/ deployement/
	cd deployement \
	  && git checkout master \
	  && git add . \
	  && git commit -m $(MSG) \
	  && git push origin master
	git add deployement
	git commit -m $(MSG)
	git push
