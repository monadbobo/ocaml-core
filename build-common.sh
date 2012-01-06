#!/bin/bash

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

function make_tags {
    cat >"$1" <<EOF
# OASIS_START
# OASIS_STOP
EOF
    cat >>"$1"
}

function tag_for_pack {
    local libname=$(capitalize "$1")
    shift
    for ml in "$@"; do
        local dir=$(dirname "$ml" | sed -e 's_^\./__')
        local bname=$(basename "$ml")
        echo "\"${dir}/${bname%%.ml*}.cmx\": for-pack($libname)"
        echo "\"${dir}/$(capitalize ${bname%%.ml*}).cmx\": for-pack($libname)"
    done
}

function enable_pack_in_setup_ml {
    awk -f - setup.ml > setup.ml.pack <<EOF
BEGIN                      { next_pack=0; skipnext=0; }
/cs_name/                  { next_pack=0; }
/cs_name = "$1"/           { next_pack=1; }
/lib_pack = (true|false);/ { if (next_pack) {
                               print "lib_pack = true;"
                               skipnext=1
                             }
                           }
/.*/                       { if (!skipnext) { print \$0; }
                             skipnext=0; }
EOF
    mv setup.ml.pack setup.ml
    for dir in $(find . -name "$1".mllib -exec dirname \{\} \;); do
        mv "$dir"/"$1".mllib "$dir"/"$1".mlpack
    done
}

function check_linux_enabled {
    enable_linux=false
    enable_linux_default="--disable-linux"
    case $(ocamlc -config | awk '$1 == "system:" {print $2}') in
        linux|linux_elf)
            enable_linux=true
            enable_linux_default="--enable-linux"
            ;;
    esac

    for opt in "$@"; do
        case "$opt" in
            --enable-linux)  enable_linux=true;  enable_linux_default= ;;
            --disable-linux) enable_linux=false; enable_linux_default= ;;
        esac
    done
}

function check_posix_timers_enabled {
    enable_timers=false
    enable_timers_default="--disable-posix-timers"
    if [[ $(getconf _POSIX_TIMERS 2>/dev/null) -ge 200112 ]]; then
        enable_timers=true
        enable_timers_default="--enable-posix-timers"
    fi

    for opt in "$@"; do
        case "$opt" in
            --enable-posix-timers)  enable_timers=true;  enable_timers_default= ;;
            --disable-posix-timers) enable_timers=false; enable_timers_default= ;;
        esac
    done
}

HERE=$(dirname "$0")
