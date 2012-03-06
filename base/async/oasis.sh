#!/bin/bash
set -e -u -o pipefail

here="$(dirname "${BASH_SOURCE[0]}")"

my_join () {
    local FIRST="true"
    while read line; do
        if [[ "$FIRST" != "true" ]]; then
            echo -n ","
        else
            FIRST="false"
        fi
        echo -n "$line"
    done
    echo
}

list_mods () {
    echo Std
    for i in $here/$1/*.ml; do
        bname="$(basename $i)"
        j=${bname%%.ml*};
        case $j in
            *);;
        esac
        echo -n "${j:0:1}" | tr "[:lower:]" "[:upper:]"; echo ${j:1};
    done
}

cat >$here/_oasis <<EOF
#AUTOGENERATED FILE; EDIT oasis.sh INSTEAD

OASISFormat:  0.2
OCamlVersion: >= 3.12
Name:         async
Version:      107.01
Synopsis:     Jane Street Capital's asynchronous execution library
Authors:      Jane street capital
Copyrights:   (C) 2008-2011 Jane Street Capital LLC
License:      LGPL-2.1 with OCaml linking exception
LicenseFile:  LICENSE
Plugins:      StdFiles (0.2),
              DevFiles (0.2),
              META (0.2)
BuildTools:   ocamlbuild
Description:  Jane Street Capital's asynchronous execution library
FindlibVersion: >= 1.2.7
XStdFilesAUTHORS: false
XStdFilesINSTALLFilename: INSTALL
XStdFilesREADME: false

Library async
  Path:               lib
  FindlibName:        async
  Pack:               true
  Modules:$(list_mods lib | sort -u | my_join)
  BuildDepends:       async_core,
                      async_unix,
                      async_extra,
                      threads


Document "async"
  Title:                Jane street's async library
  Type:                 ocamlbuild (0.2)
  BuildTools+:          ocamldoc
  XOCamlbuildPath:      lib
  XOCamlbuildLibraries: async

EOF

cat >$here/_tags <<EOF
# OASIS_START
# OASIS_STOP
# <lib_test/*.ml{,i}>: syntax_camlp4o
EOF

cd $here
oasis setup
./configure "$@"

