#!/usr/bin/env bash

THIS_DIR="$(dirname "$(readlink -f "$0")")"
cd "${THIS_DIR}/src"

ocamlbuild -use-menhir main_scan.native
ocamlbuild -use-menhir main_tokens.native
