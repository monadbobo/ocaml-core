#!/usr/bin/env bash
set -e -u
LIB=pa_ounit
[[ "$#" != 1 ]] && { echo "usage: $0  FILE_TO_PREPROCESS" >&2; exit 1; }
[[ -f "$(hg root)/lib/$LIB.cmo" ]] || {
   echo "$LIB.cmo needs to be built for this script to run" >&2;
   exit 1
}
"$(jomake --ocaml-path)/bin/camlp4o" -printer 'Camlp4OCamlPrinter' -I "$(hg root)/lib" $LIB.cmo pa_type_conv.cmo pa_sexp_conv.cmo $1
