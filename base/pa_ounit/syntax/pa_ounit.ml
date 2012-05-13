open Camlp4.PreCast

(**
 How this works:
   This syntax extension does two things:
   - Turn toplevel expressions in proper tests (by wrapping them in functions
   and using filenames/line_numbers)
   _ Collect all the tests in a file automatically

   Since we are trying to play nicely with other extensions we try to not
   override any rules in the grammar. In particular we cannot override the
   module creation rule the way type_conv does it. In order to collect all tests
   we insert "breadcrumbs" items in the modules that have a very particular
   shape when generating the tests and then collect those breadcrumbs by folding
   over the AST.

   TODO: think about how to make the fold cheaper.
*)

(** Used to lazily initialise the Random number generator *)
let init = ref false

(** This is an optimisation: this variable tells us whether the file we
    processed contained any tests. This is useful to avoid paying the full cost
    of a fold when the processed file does not contain any tests. *)
let has_tests = ref false

let uid s =
  if not !init then begin
    Random.self_init ();
    init := true
  end;
  Printf.sprintf "__Pa_ounit_%s_%i" s (Random.bits ())

let breadcrumb_id =
  lazy (uid "internal")

let breadcrumb exp =
  let loc = Ast.loc_of_expr exp in
  Ast.StExp (loc,Ast.ExApp
    (loc,Ast.ExId(loc,Ast.IdLid (loc,Lazy.force breadcrumb_id)),exp))

let label name e=
  let loc = Ast.loc_of_expr e in
  <:expr@loc< OUnit.TestLabel $str:name$ $e$ >>

let chop_extension fn =
  try
    let stop = String.index fn '.' in
    String.sub fn 0 stop
  with Not_found -> fn

let module_name_of_file fn =
  String.capitalize (chop_extension (Filename.basename fn))

let rec add_in_sig to_add = function
  | <:module_type@loc< sig $si$ end >> ->
    <:module_type@loc< sig $si$; $to_add$ end >>
  | Ast.MtNil loc ->
    <:module_type@loc< sig $to_add$; end >>
  |(* (<:module_type@loc< $id:_$ >>
     | <:module_type@loc< $_$ with $_$>>
     | Ast.MtAnt (loc,_)
     | <:module_type@loc< functor ($uid:_$:$_$) -> $_$ >>
     | <:module_type@loc< '$_$ >>
     | _ ) *) (* MtOf (_,_) is in 3.12.1 but not 3.12.0. We do not check for
                 exhaustiveness *)
     mt ->
    let loc = Ast.loc_of_module_type mt in
    <:module_type@loc< sig include $mt$; $to_add$; end >>

let append_to_list ~list e =
  let loc = Ast.loc_of_expr e in
  match list with
  | Some v -> list, <:str_item@loc< value $lid:v$ () :  list OUnit.test =
      [ $e$ :: $lid:v$ () ];  >>
  | None ->
    let bname = String.lowercase
      (chop_extension (Filename.basename (Loc.file_name loc)))
    in
    (* TODO: use a uuid *)
    let id = uid (bname^"_test_list") in
    Some id,<:str_item@loc< value $lid:id$ () : list OUnit.test = [ $e$ ];  >>

(* We use the ast mapper to reach all the deep nested modules that might
   be in expressions etc.. *)
let ast_mapper = object (self)
  inherit Ast.map
  method module_expr me = snd (self#module_expr' me)

  method str_item' ~list = function
  (* Bread crumb *)
  | Ast.StExp (_,Ast.ExApp
    (_,Ast.ExId(_,Ast.IdLid (_,fid)),e)) when fid = Lazy.force breadcrumb_id ->
    append_to_list ~list e
  | <:str_item@loc< $s1$;$s2$ >> ->
    let list,s1 = self#str_item' ~list s1 in
    let list,s2 = self#str_item' ~list s2 in
    list,<:str_item@loc< $s1$;$s2$ >>
  | <:str_item@loc< module $uid:name$ = $me$ >> ->
    let has_tests,me = self#module_expr' me in
    let str_item = <:str_item@loc< module $uid:name$ = $me$ >> in
    if has_tests then
      let test =
        <:expr@loc< OUnit.TestLabel $str:name$ ($uid:name$.ounit_tests ()) >>
      in
      let list,test_it = append_to_list ~list test in
      list,<:str_item@loc< $str_item$; $test_it$ >>
    else
      list,str_item
  | str_item -> list,self#str_item str_item
(* Returns true if the module a ounit_tests component*)

  method module_expr' = function
  | (Ast.MeNil _
     | Ast.MeAnt _ (* $antiquot$ *)
     | <:module_expr< $id:_$ >>) as v ->  false,v
  | <:module_expr@loc< (value $(e:Ast.expr)$) >>  ->
    false,<:module_expr@loc< (value $(self#expr e)$) >>
  | <:module_expr@loc< ($me$:$mt$) >> ->
    let has_tests,me = self#module_expr' me in
    let mt = self#module_type mt in
    if has_tests then
      let ounit_test_sig_item =
        <:sig_item@loc< value ounit_tests:unit -> OUnit.test >>
      in
      let mt = add_in_sig ounit_test_sig_item mt in
      true,<:module_expr@loc< ($me$:$mt$) >>
    else
      false,<:module_expr@loc< ($me$:$mt$) >>
  | <:module_expr@loc< $m1$ $m2$ >> ->
    false,<:module_expr@loc< $self#module_expr m1$ $self#module_expr m2$ >>
  | <:module_expr@loc< functor ($name$:$ty$) -> $me$ >> ->
    false,<:module_expr@loc< functor ($name$:$ty$) ->
      $snd (self#module_expr' me)$ >>
  | <:module_expr@loc<struct $it$ end >> ->
    let (list,it) =  self#str_item' ~list:None it in
    match list with
    | Some id ->
      true,<:module_expr@loc<struct
          $it$;
          value ounit_tests () : OUnit.test =
            OUnit.TestList (List.rev ($lid:id$ ()));
      end >>
    | None -> false,<:module_expr@loc<struct $it$ end >>
end

let gen_ounit_tests si =
  let loc = Ast.loc_of_str_item si in
  let list,si =
    if !has_tests then
      ast_mapper#str_item' ~list:None si
    else
      None,si
  in
  let tests = match list with
    | None -> <:expr@loc< [ ] >>
    | Some v -> <:expr@loc< List.rev ($lid:v$ ()) >>
  in
  let name = Loc.file_name loc in
  let expr =
    label name <:expr@loc< OUnit.TestList $tests$ >>
  in
  <:str_item@loc< $si$; value ounit_tests () : OUnit.test = $expr$ >>

let () =
  AstFilters.register_str_item_filter gen_ounit_tests

let () =
  AstFilters.register_sig_item_filter begin fun si ->
    let loc = Ast.loc_of_sig_item si in
    <:sig_item@loc< $si$; value ounit_tests : unit -> OUnit.test >>
  end

let test_expr e =
  let loc = Ast.loc_of_expr e in
  <:expr@loc< fun [ () -> (OUnit.TestCase (fun [ () -> $e$]))] >>

let syntax_printer =
  let module PP = Camlp4.Printers.OCaml.Make (Syntax) in
  new PP.printer ()

let string_of_expr expr =
  let buffer = Buffer.create 16 in
  Format.bprintf buffer "%a%!" syntax_printer#expr expr;
  Buffer.contents buffer

let string_of_me me  =
  let buffer = Buffer.create 16 in
  Format.bprintf buffer "%a%!" syntax_printer#module_expr me;
  Buffer.contents buffer

let rec short_desc_of_expr ~max_len = function
  | (<:expr< let $_$ in $e$ >>
        | <:expr< let rec $_$ in $e$ >>
        | <:expr< let module $_$ = $_$ in $e$ >>)
    -> short_desc_of_expr ~max_len e
  | e ->
    let s = string_of_expr e in
    let len = String.length s in
    let res =
      if  len >= max_len then
        let s_short = String.sub s 0 (max_len - 5) in
        s_short ^ "[...]"
      else
        s
    in
    for i=0 to String.length res -1 do
      if res.[i]='\n' then
        res.[i] <- ' '
    done;
    res

let str_item = Syntax.str_item
EXTEND Gram
  GLOBAL: str_item;
  str_item:
    [[
     "TEST"; id = OPT Syntax.a_STRING; "=" ; e = Syntax.expr ->
        has_tests := true;
        let short () = short_desc_of_expr ~max_len:50 e
        and filename = Loc.file_name _loc
        and line = Loc.start_line _loc in
        let srep = Printf.sprintf "%s:line %i %s"
          filename
          line
          (String.escaped (string_of_expr e))
        in
        let eloc = Ast.loc_of_expr e in
        let e = <:expr@eloc<
          if not ($e$ : bool) then failwith $str:srep$ else ()
        >>
        and id =
          match id with
          | None -> Printf.sprintf "line %i <<%s>>" line
            (String.escaped (short ()))
          | Some v -> v
        in
        breadcrumb
          (label id <:expr< OUnit.TestCase (fun [ () -> $e$]) >>)
    | "TEST_UNIT"; id = OPT Syntax.a_STRING; "=" ; e = Syntax.expr ->
        has_tests := true;
        let short () = short_desc_of_expr ~max_len:50 e
        and eloc = Ast.loc_of_expr e in
        let e = <:expr@eloc<($e$ : unit)>>
        and id =
          match id with
          | None ->
            Printf.sprintf "line %i <<%s>>" (Loc.start_line _loc)
              (String.escaped (short ()))
          | Some v -> v
        in
        breadcrumb
          (label id <:expr< OUnit.TestCase (fun [ () -> $e$]) >>)
    | "TEST_MODULE";
         id = OPT Syntax.a_STRING ; "=" ; orig = Syntax.module_expr ->
         has_tests := true;
         let me = ast_mapper#module_expr
           <:module_expr< ($orig$:sig
                                  value ounit_tests : unit -> OUnit.test;
                                end) >>
         and e =  <:expr<M.ounit_tests ()>> in
         let e = match id with
           | None ->
             let mes = string_of_me orig in
             if String.contains mes ' ' then
               e
             else
               label mes e
           | Some v -> label v e
         in
         breadcrumb
          <:expr< let module M = $me$ in $e$ >>
    ]];
END
