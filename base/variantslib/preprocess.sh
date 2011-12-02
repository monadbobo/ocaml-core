#!/bin/bash
set -e -u
[[ "$#" != 1 ]] && { echo "usage: $0  FILE_TO_PREPROCESS" >&2; exit 1; }
[[ -f "$(hg root)/lib/pa_variants_conv.cmo" ]] || {
   echo "pa_variants_conv.cmo needs to be built for this script to run" >&2;
   exit 1
}
"$(jomake --ocaml-path)/bin/camlp4o" -printer 'Camlp4OCamlPrinter' -I "$(hg root)/lib" pa_type_conv.cmo pa_variants_conv.cmo $1
