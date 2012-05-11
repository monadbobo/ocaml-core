#!/bin/bash

core_version=108.00-pre1

# in dependency-topological order
build_order=(
    type_conv
    pipebang
    compare
    typehash
    fieldslib
    variantslib
    pa_ounit
    bin-prot
    sexplib
    core
    core/extended
    async/core
    async/unix
    async/extra
    async
)

function my_join {
    sep=","
    if [[ $# -gt 0 ]]; then sep="$1"; fi
    local cursep=
    while read line; do
        echo -n "$cursep"
        cursep="$sep"
        echo -n "$line"
    done
    echo
}

function capitalize {
    local j="$1"
    echo -n "${j:0:1}" | tr "[:lower:]" "[:upper:]"; echo ${j:1}
}

function mod_names {
    while read line; do
        bname=$(basename "$line")
        j=${bname%%.ml*};
        if [[ "$j" == "inline_tests_runner" ]]; then continue; fi
        capitalize "$j"
    done
}

function list_mods {
    find "$1" -name '*.ml' -print | mod_names | sort -u | my_join
}

function list_stubs {
    find "$1" -name "*.[ch]" -exec basename \{\} \; | sort -u | my_join
}

function make_tags {
    cat >"$1" <<EOF
# OASIS_START
# OASIS_STOP
EOF
    cat >>"$1"
}

function make_myocamlbuild {
    cat >"$1" <<EOF
(* OASIS_START *)
(* OASIS_STOP *)
EOF
    cat >>"$1"
}

function make_setup_ml {
    cat >"$1" <<EOF
(* OASIS_START *)
(* OASIS_STOP *)
let _ = setup

EOF
    cat >> "$1"
    cat >> "$1" <<EOF

let () = BaseSetup.setup setup_t
EOF
}

HERE=$(dirname -- "$0")

