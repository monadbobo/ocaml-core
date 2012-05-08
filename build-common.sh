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

function make_myocamlbuild {
    cat >"$1" <<EOF
(* OASIS_START *)
(* OASIS_STOP *)
EOF
    cat >>"$1"
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

function getconf_or_zero {
    local ret=$(getconf "$@" 2>/dev/null)
    case "$ret" in
	[0-9][0-9]*) echo "$ret" ;;
	*) echo 0 ;;
    esac
}

function check_posix_timers_enabled {
    enable_timers=false
    enable_timers_default="--disable-posix-timers"
    if [[ $(getconf_or_zero _POSIX_TIMERS) -ge 200112 ]]; then
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

function declare_tests_flag {
  # prior to oasis version 0.3.0, the "tests" flag is not built in
  # and, the "version" argument is not supported.
  if ! oasis version 2>/dev/null >/dev/null; then
    cat <<EOF
Flag tests
  Description: Build and run tests
  Default:     false
EOF
  fi
}

HERE=$(dirname "$0")
