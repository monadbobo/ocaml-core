#!/bin/bash

core_version=108.00-pre2

# in dependency-topological order
build_order=(
    base/type_conv
    base/pipebang
    base/compare
    base/typehash
    base/fieldslib
    base/variantslib
    base/pa_ounit
    base/bin_prot
    base/sexplib
    base/core
    base/core/extended
    base/async/core
    base/async/unix
    base/async/extra
    base/async
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

useful_ocaml_functions='
let protectx x ~f ~finally =
  let r = try f x with exn -> finally x; raise exn in
  finally x; r

let read_lines ic =
  let rec loop acc =
    match try Some (input_line ic) with End_of_file -> None with
    | Some line -> loop (line :: acc)
    | None -> List.rev acc
  in loop []

let test cmd =
  match Sys.command cmd with
  | 0 -> true
  | 1 -> false
  | _ -> failwith ("command '"^cmd^"' failed.")

let sh_lines cmd =
  protectx (Filename.temp_file "ocamlbuild_cmd" ".txt")
    ~f:(fun fn ->
      ignore (Sys.command ("(" ^ cmd ^ ") >" ^ fn) : int);
      protectx (open_in fn) ~f:read_lines ~finally:close_in)
    ~finally:Sys.remove

let getconf var =
  let cmd = Printf.sprintf "getconf %S" var in
  match sh_lines cmd with
  | []  -> None
  | [x] -> Some x
  | _   -> failwith ("`"^cmd^"` returned multiple lines")

let endswith x s =
  let len_x = String.length x and len_s = String.length s in
  (len_x <= len_s) && x = String.sub s (len_s - len_x) len_x

let select_files dir ext =
  List.map (Filename.concat dir)
    (List.filter (endswith ext)
      (Array.to_list (Sys.readdir dir)))
;;
'

function make_setup_ml {
    cat >"$1" <<EOF
(* OASIS_START *)
(* OASIS_STOP *)
let _ = setup

$useful_ocaml_functions

EOF
    cat >> "$1"
    cat >> "$1" <<EOF

let () = BaseSetup.setup setup_t
EOF
}

function make_myocamlbuild {
    cat >"$1" <<EOF
(* OASIS_START *)
(* OASIS_STOP *)

$useful_ocaml_functions

let setup_standard_build_flags () =
    begin match getconf "LFS64_CFLAGS" with
    | None -> ()
    | Some flags -> flag ["compile"; "c"] (S[A"-ccopt"; A flags])
    end;
    let cflags =
      let flags =
        [
          "-pipe";
          "-g";
          "-fPIC";
          "-O2";
          "-fomit-frame-pointer";
          "-fsigned-char";
          "-Wall";
          "-pedantic";
          "-Wextra";
          "-Wunused";
(*          "-Werror"; *)
          "-Wno-long-long";
        ]
      in
      let f flag = [A "-ccopt"; A flag] in
      List.concat (List.map f flags)
    in
    flag ["compile"; "c"] (S cflags);
    flag ["compile"; "ocaml"] (S [A "-w"; A "@Aemr-28"; A "-strict-sequence" ])
;;

EOF
    cat >>"$1"
}

function make_myocamlbuild_default {
    make_myocamlbuild "$1" <<EOF
let dispatch = function
  | After_rules as e ->
    setup_standard_build_flags ();
    dispatch_default e
  | e -> dispatch_default e

let () = Ocamlbuild_plugin.dispatch dispatch
EOF
}

HERE=$(cd "$(dirname -- "$0")"; pwd);
