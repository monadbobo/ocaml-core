include Printf


(**
   failwith, invalid_arg, and exit accepting printf's format.
*)

let failwithf f = ksprintf (fun s () -> failwith s) f

let invalid_argf f = ksprintf (fun s () -> invalid_arg s) f

let exitf f =
  ksprintf (fun s () -> Printf.eprintf "%s\n%!" s; exit 1) f;
;;
