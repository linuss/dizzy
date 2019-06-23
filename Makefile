default: all

.PHONY: test cover

ELM_FILES = $(shell find src -name '*.elm')

SHELL := /bin/bash
NPM_PATH := ./node_modules/.bin
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
DIST_DIR := ./lib/dist

export PATH := $(NPM_PATH):$(PATH)

all: elm

analyse:
	@elm-analyse --serve --elm-format-path=./node_modules/elm-format/bin/elm-format src

clean:
	@rm -Rf dist/*

elm:
	@elm make src/Main.elm --debug --output dist/main.js

elmoptimized:
	@elm make --optimize src/Main.elm --output dist/main.js

deps:
	@npm install

distclean: clean
	@rm -Rf elm-stuff
	@rm -Rf tests/elm-stuff
	@rm -Rf node_modules

format:
	@elm-format --yes src
	@elm-format --yes tests

format-validate:
	@elm-format --validate src
	@elm-format --validate tests

help:
	@echo "Run: make <target> where <target> is one of the following:"
	@echo "  all                    Compile all Elm files"
	@echo "  analyse                Run Elm analyse"
	@echo "  clean                  Remove 'dist' folder"
	@echo "  deps                   Install build dependencies"
	@echo "  distclean              Remove build dependencies"
	@echo "  format                 Run Elm format"
	@echo "  format-validate        Check if Elm files are formatted"
	@echo "  help                   Magic"
	@echo "  test                   Run Elm-test"
	@echo "  watch                  Run 'make all' on Elm file change"

minify:
	@npx uglify-js ${DIST_DIR}/main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | npx uglify-js --mangle --output=${DIST_DIR}/main.js\

test:
	@elm-test --compiler ${ROOT_DIR}/node_modules/.bin/elm

watch:
	find src -name '*.elm' | entr make all
