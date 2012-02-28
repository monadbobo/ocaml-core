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
    # if this is not the patched version of oasis, we need to patch it.
    if ! grep -q lib_pack setup.ml; then
	patch -l -p1 <<EOF
--- a/setup.ml	2012-01-26 09:04:49.000000000 +0000
+++ b/setup.ml	2012-01-26 09:25:06.000000000 +0000
@@ -992,6 +992,7 @@
   type library =
       {
         lib_modules:            string list;
+        lib_pack:               bool;
         lib_internal_modules:   string list;
         lib_findlib_parent:     findlib_name option;
         lib_findlib_name:       findlib_name option;
@@ -1285,6 +1286,7 @@
         source_file_exists is_native ext_lib ext_dll =
     (* The headers that should be compiled along *)
     let headers =
+      if lib.lib_pack then [] else
       List.fold_left
         (fun hdrs modul ->
            try
@@ -1323,11 +1325,18 @@

     (* Compute what libraries should be built *)
     let acc_nopath =
+      (* Add the packed header file if required *)
+      let add_pack_header acc =
+        if lib.lib_pack then
+          [cs.cs_name^".cmi"] :: acc
+        else
+          acc
+      in
       let byte acc =
-        [cs.cs_name^".cma"] :: acc
+        add_pack_header ([cs.cs_name^".cma"] :: acc)
       in
       let native acc =
-        [cs.cs_name^".cmxa"] :: [cs.cs_name^(ext_lib ())] :: acc
+        add_pack_header ([cs.cs_name^".cmxa"] :: [cs.cs_name^(ext_lib ())] :: acc)
       in
         match bs.bs_compiled_object with
           | Native ->
@@ -4261,6 +4270,7 @@
                 List.rev_append lib_extra acc
               in
               let acc =
+                if lib.lib_pack then acc else
                 (* Add uncompiled header from the source tree *)
                 let path =
                   BaseFilePath.of_unix bs.bs_path

EOF
	awk -f - setup.ml > setup.ml.pack <<EOF
{ if (match(\$0, /lib_internal_modules =/)) { print "lib_pack = false;"; }
  print \$0; }
EOF
	mv setup.ml.pack setup.ml
    fi
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
