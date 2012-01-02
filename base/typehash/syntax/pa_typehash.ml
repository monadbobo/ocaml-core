open Camlp4.PreCast
module Gen = Pa_type_conv.Gen

let typehash_function_name name  =
  "typehash_of_" ^ name

let body_name name =
  "__typehash_" ^ name ^ "_body"

let base_types =
  [
    [ "nativeint" ];
    [ "int64" ];
    [ "int32" ];
    [ "char" ];
    [ "int" ];
    [ "bool" ];
    [ "string" ];
    [ "float" ];
    [ "list" ];
    [ "array" ];
    [ "unit" ];
  ]
type config = {
    typ : [`Normal | `Alias | `Abstract of string] ;
    version:string option;
    simplify : bool;
    unsafe : bool;
  }

let errf loc fmt =
  Printf.ksprintf
    (fun s ->
      let filename = Loc.file_name loc
      and line = Loc.start_line loc in
      Printf.eprintf
        "File %s, line: %i\n%s\n%!"
        filename
        line
        s;
      exit 1)
    fmt

let rec index acc el = function
  | [] -> raise Not_found
  | x::_ when x = el -> acc
  | _::lst -> index (acc + 1) el lst

let index (type el) (needle:el) (haystack:el list) : int =
  index 0 needle haystack

let rec ty_dcl_constraint_loop ~params ~name ~loc =
  match params with
  | [] -> <:ctyp@loc< $lid:name$ >>
  | ty::t ->
    let base =
      ty_dcl_constraint_loop
        ~name
        ~loc
        ~params:t
    in
    let loc = Ast.loc_of_ctyp ty in
    <:ctyp@loc< $base$ _ >>

let typ_constraint ~params ~name ~loc =
  <:ctyp@loc< $ty_dcl_constraint_loop ~params ~name ~loc$ >>

let cast_int_to_internal expr =
  let loc = Ast.loc_of_expr expr in
  <:expr@loc< (Obj.magic ($expr$:int) : Typehash.internal _) >>

let hash expr =
  let loc = Ast.loc_of_expr expr in
  <:expr@loc<Hashtbl.hash_param max_int max_int $expr$ >>

let to_hash = function
  (* Hash is idempotent... *)
  | <:expr< $int:_$ >> as expr -> cast_int_to_internal expr
  | expr -> cast_int_to_internal (hash expr)

let to_int = function
  | <:expr< $int:_$ >> as expr -> expr
  | expr -> hash expr

let lit ~config loc name : Ast.expr =
  if config.simplify then
    <:expr@loc< $int:string_of_int (Hashtbl.hash name)$ >>
  else
    <:expr@loc< $str:name$ >>

let prod ~config ~loc ?name exprs =
  let exprs = match name with
    | Some s -> lit ~config loc s :: exprs
    | None   -> exprs
  in
  let exp = <:expr@loc< ( $tup:Ast.exCom_of_list exprs$ ) >> in
    (* Tries to simplify the generated expression to a single int if possible *)
  if config.simplify then (* Hashtbl.hash of a list is the same as
                           Hashtbl.hash of the tuple of the elements in that
                           list *)
    try
      let v = List.map (function
        | <:expr<$int:i$>> -> i
        | _ -> raise Exit
      ) exprs
      in
      <:expr@loc< $int:string_of_int (Hashtbl.hash v)$ >>
    with Exit -> exp
  else
    exp

module Config = struct

  let empty = {
    typ = `Normal;
    simplify = true;
    version = None;
    unsafe = false;
  }

  let gram_entry : config Gram.Entry.t  = Gram.Entry.mk "typehash_arguments"

  EXTEND Gram
    GLOBAL: gram_entry;
    typehash_arg: [[
      LIDENT "version"; "=" ; version = STRING -> (function
        | { typ = `Alias } ->
          failwith "typehash: \"version\" would defeat the purpose of \
                     \"alias\"";
        | acc -> { acc with version = Some version })
    | LIDENT "debug"  -> (fun acc -> { acc with simplify = false })
    | LIDENT "abstract"; "="; uid = STRING -> (function
        | { typ = `Alias } ->
         failwith "typehash: \"abstract\" and \"alias\" are mutually exclusive";
        | acc -> { acc with typ = `Abstract uid})
    | LIDENT "alias" -> (function
        | { typ = `Abstract _ } ->
          failwith "typehash: \"abstract\" and \"alias\" are mutually \
                    exclusive";
        | { version = Some _ } ->
          failwith "typehash: \"version\" would defeat the purpose of \
                              \"alias\"";
        | acc -> { acc with typ = `Alias});
    | LIDENT "unsafe" -> (function v -> {v with unsafe=true});
    | id = LIDENT ->
      failwith (Printf.sprintf "Unknown typehash argument %S" id);
    ]];

    gram_entry: [[
      v = LIST0 typehash_arg SEP ";" ; EOI ->
        ListLabels.fold_left v
          ~f:(fun acc f -> f acc)
          ~init:empty
    ]];
  END

end

module Body = struct

  type acc = {
    (* References to external seen in this type definition *)
    (* rev_path * arity *)
    mutable external_types     : (string list * int) list;
    mutable external_types_cnt : int;
    type_lu_tbl                : (string list,string) Hashtbl.t;
    (* type_vars : string list; *)
  }

  type typedef = {
    loc           : Ast.Loc.t;
    body          : Ast.ctyp;
    params        : Ast.ctyp list;
    name          : string;
    internal_name : string
  }

  let rec list_of_app_ctyp = function
    | <:ctyp< $tp1$ $tp2$ >> -> tp2 :: list_of_app_ctyp tp1
    | tp -> [tp]

  let rec get_type_defs__loop idx = function
    | Ast.TyDcl (loc, name, tps, rhs, cl) as ty ->
      if cl <> [] then
        Gen.error
          ~fn:"split_types"
          ~msg:"cannot handle types with constraints" ty;
      [{
        loc = loc;
        name = name;
        params = tps;
        body = rhs;
        internal_name = "local" ^ (string_of_int idx)
      }],idx+1
    | <:ctyp< $tp1$ and $tp2$ >> ->
      let l,idx= get_type_defs__loop idx tp1 in
      let r,idx = get_type_defs__loop idx tp2 in
      l@r,idx
    | ty -> Gen.unknown_type ty "Pa_typehash.Body.get_typenames"

  let get_type_defs ty = fst (get_type_defs__loop 0 ty)

  let rev_path_to_string rp = String.concat "." (List.rev rp)

  let rec typehash_body
      ?(arity=0)
      ~type_vars
      ~config
      ~info : Ast.ctyp -> Ast.expr =
    function
      | <:ctyp< $_$ == $tp$ >> |  <:ctyp< private $tp$ >>
        -> typehash_body ~config ~type_vars ~info tp
      | <:ctyp< ( $tup:tp$ ) >> -> prod_type "tuple" ~config ~type_vars ~info tp
      | <:ctyp< { $flds$ } >> ->
        prod_type "record" ~config ~type_vars ~info flds
      | <:ctyp@loc< $lid:name$ : mutable $tp$ >>
      | <:ctyp@loc< $lid:name$ : $tp$ >> ->
        prod_type ~config ~type_vars ~loc ("lbl:" ^ name) ~info tp
      | <:ctyp< `$cnstr$ of $tp$ >> | <:ctyp< $uid:cnstr$ of $tp$ >>->
        prod_type ~config ~type_vars ("constr:"^cnstr) ~info tp
      | <:ctyp@loc< `$cnstr$ >> | <:ctyp@loc< $uid:cnstr$ >> ->
        lit ~config loc ("constr:"^cnstr)
      | <:ctyp< [ $tp$ ] >>
      | <:ctyp< [= $tp$ ] >>
      | <:ctyp< [< $tp$] >>
      | <:ctyp< [> $tp$] >> -> prod_type ~config ~type_vars "tysum" ~info tp
      | <:ctyp@loc< $_$ $_$ >> as ty ->
        (match List.rev (list_of_app_ctyp ty) with
        | [] -> assert false
        | h::t ->
          let arity = List.length t in
          let tyl =
            (typehash_body ~config ~arity ~type_vars ~info h)
            ::List.map (typehash_body ~config ~type_vars ~info) t
          in
          prod ~name:"tyapp" ~loc ~config (List.rev tyl))
      | <:ctyp@loc< $id:id$ >> ->
        let rev_path = Gen.get_rev_id_path id [] in
        let name = begin
          try
            Hashtbl.find info.type_lu_tbl rev_path
          with Not_found ->
            let name =
              "external" ^ (string_of_int info.external_types_cnt)
            in
            info.external_types_cnt <- info.external_types_cnt +1;
            info.external_types <- (rev_path,arity) :: info.external_types;
            Hashtbl.add info.type_lu_tbl rev_path name;
            name
        end
        in
        lit ~config loc name
      | <:ctyp@loc< '$x$ >> as ty ->
        let idx =
          try
            index x type_vars
          with Not_found -> Gen.error ty
            ~fn:"typehash_body"
            ~msg:"free type variable"
        in
        lit ~config loc ("var:" ^ string_of_int idx)
      | Ast.TyNil _ as ty ->
        errf (Ast.loc_of_ctyp ty)
          "ERROR:cannot handle Abstract types transparently; please define \
 your abstract type by itself with an abstract qualifier like:\n\n\
     type t with typehash(abstract=\"Pcre.org.regexp\")\n
It is recommended to use an url or a findlib package to ensure uniqueness of \
generated hashes.
 "
      | tp -> Gen.unknown_type tp "typehash_body"

  and prod_type ~config ~type_vars ~info ?loc name tp : Ast.expr =
    let loc = match loc with
      | Some v -> v
      | None -> Ast.loc_of_ctyp tp
    and exprs =
      List.map
        (typehash_body ~config ~type_vars ~info)
        (Ast.list_of_ctyp tp [])
    in
    prod ~config ~loc ~name exprs

  let rec apply_anomymous_ty_args ty = function
    | 0 -> ty
    | n ->
      let loc = Ast.loc_of_ctyp ty in
      apply_anomymous_ty_args
      <:ctyp@loc< $ty$ _ >>
        (n-1)

  (* The name of the body of an external reference. *)
  let ty_ref loc arity = function
    | (tn :: path) as full_path ->
      let ty_id = Gen.ident_of_rev_path loc full_path in
      let ty = apply_anomymous_ty_args
        <:ctyp@loc< $id:ty_id$ >>
          arity
      in
      <:expr@loc<
        ($id:Gen.ident_of_rev_path loc (body_name tn :: path)$
        : Typehash.internal $ty$)
      >>
    | [] -> assert false

  let version ~config expr =
    match config.version with
    | None -> expr
    | Some v ->
      let loc = Ast.loc_of_expr expr in
      prod [expr]
        ~config
        ~loc
        ~name:("version:" ^ v)

  let alias_args_error ty msg =
    Gen.error ty
      ~fn:"typehash:check alias arguments"
      ~msg:("The type arguments must be the same on the left and right for a \
 type alias.\n"^msg)

  let rec check_alias_list l r =
    match l,r with
    | [],[] -> ()
    | (<:ctyp< '$x1$ >>::l),(<:ctyp< '$x2$ >> as ty::r) ->
      if x1 <> x2 then
        alias_args_error ty ("This should have been:'"^x1);
      check_alias_list l r
    | (<:ctyp< '$x1$ >>::_),(ty::_) ->
      alias_args_error ty ("This argument should have been '" ^x1)
    | (ty::_),(_::_) ->
      Gen.unknown_type ty "check_alias_list"
    | (ty::_),[] ->
      alias_args_error ty "Extra argument on the right"
    | [],(ty::_) ->
      alias_args_error ty "Extra argument on the left"

  let rec unalias = function
    | <:ctyp< $ty$ == $_$ >> | <:ctyp< private $ty$ >> -> unalias ty
    | ty ->
      begin match List.rev (list_of_app_ctyp ty) with
      | (<:ctyp< $id:id$>>)::args -> id,args
      | ty::_ -> Gen.error ~fn:"typehash::alias" ~msg:"not a type alias" ty
      | [] -> assert false
      end

  let generate_alias_body ~config:_ ~loc:_ ty typedefs =
    match typedefs with
    | _::_::_ | [] ->
      Gen.error
        ty
        ~fn:"typedef::alias"
        ~msg:"Can only generate alias for non-recursively defined types"
    | [{body;params;loc}] ->
      let id,rparams = unalias body in
      check_alias_list params rparams;
      let path = Gen.get_rev_id_path id [] in
      ty_ref loc (List.length params) path

  let generate_normal_body ~config ~loc typedefs =
    let type_lu_tbl =  Hashtbl.create 16 in
    (* Add the builtins *)
    List.iter
      (fun path -> Hashtbl.add type_lu_tbl path (rev_path_to_string path))
      base_types;
    List.iter
      (fun td -> Hashtbl.add type_lu_tbl [td.name] td.internal_name)
      typedefs;
    let info =
      { external_types = [];
        type_lu_tbl = type_lu_tbl;
        external_types_cnt = 0; }
    in
    let compiled_typedefs =
      List.map
        (fun td ->
          let type_vars = List.map Gen.get_tparam_id td.params in
          let body = typehash_body ~config ~type_vars ~info td.body in
          let loc = td.loc in
          prod ~config ~loc ~name:("tydecl:"^td.internal_name) [body])
        typedefs
    in

    (* The kernel is the base representation of this specific typedef
       it does not depend on any other type representation (that's what
       the body is for)
    *)
    let kernel = match compiled_typedefs with
      | [v] -> v
      | l ->
        prod
          ~config
          ~loc
          ~name:"and"
          l
    in
    let kernel = version ~config kernel in
    match info.external_types with
    | [] -> kernel
    | l->
      let tdefs =
        List.rev_map (fun (path,arity) -> ty_ref loc arity path) l
      in
        <:expr@loc< ( $Ast.exCom_of_list tdefs$, $kernel$) >>

  let generate_abstract_body ~config ~loc ~name typedefs =
    if not config.unsafe then begin
      match typedefs with
      | [ {body=Ast.TyNil _} ] -> ()
      | _ ->
        errf loc "The abstract typehash keyword is supposed to be use on a \
single non-reccursive abstract type:
 type t with typehash(abstract=\"my_url.t\")
You can disable this error by adding \"unsafe\" to the typehash arguments"
    end;
    let kernel = lit ~config loc ("abstract:" ^ name) in
    version ~config kernel

  let generate config ty : typedef list * Ast.expr =
    let loc = Ast.loc_of_ctyp ty in
    let typedefs = get_type_defs ty in
    let body =
      match config.typ with
      | `Normal -> generate_normal_body ~config ~loc typedefs
      | `Abstract name ->
         generate_abstract_body ~config ~loc ~name typedefs
      | `Alias ->
         generate_alias_body ~config ~loc ty typedefs
    in
    List.rev typedefs , body

end

module Gen_struct = struct

  let bid = body_name
  let extid name = typehash_function_name name

  let publish expr ty =
    let loc = Ast.loc_of_expr expr in
    <:expr@loc< ($expr$ : Typehash.internal $ty$) >>

  let generate ~config ty =
    let typedefs,body = Body.generate config ty in
    let bodys = match typedefs with
      | [] -> assert false
      | [{Body.name;loc;params} as td] ->
        [ <:str_item@loc< value $lid:bid td.Body.name$ :
            Typehash.internal $typ_constraint ~name ~loc ~params$ =
            $to_hash body$ >> ]
      | l ->
        let b_strit,bk =
          match body with
          | <:expr<$int:_$>> as e  -> [],e
          | e  ->
            (* A common body shared between several definitions...*)
            let loc = Ast.loc_of_expr e in
            [ <:str_item@loc< value __typehash_typedef_body : int = $to_int e$
            >> ],
            <:expr@loc< __typehash_typedef_body >>
        in
        b_strit
        @ List.map (fun ({Body.name;loc;params} as td) ->
          <:str_item@loc<
            value $lid:bid td.Body.name$ :
            Typehash.internal $typ_constraint ~name ~loc ~params$ =
            $to_hash
              (prod ~config ~loc ~name:td.Body.internal_name [bk])$; >>)
          l
    in
    let decls =
      bodys
      @ List.map (function
        | {Body.name=name;params=[];loc=loc} ->
          let ty = typ_constraint ~loc ~params:[] ~name in
          let base =
            <:str_item@loc<
              value $lid:extid name$ : Typehash.t $ty$
            = Obj.magic ($lid:bid name$ : Typehash.internal $ty$) >>
          in
          if name = "t" then
            <:str_item@loc< $base$; value typehash = $lid:extid name$ >>
          else
            base
        | {Body.loc=loc} -> <:str_item@loc< >>)
        typedefs
    in
    Ast.stSem_of_list decls

end

module Gen_sig = struct

  let rec sig_of_td__loop acc = function
    | [] ->
      let loc = Ast.loc_of_ctyp acc in
      <:ctyp@loc< Typehash.t $acc$ >>
    | tp :: tps ->
      let tp = Gen.drop_variance_annotations tp in
      let loc = Ast.loc_of_ctyp tp in
      let left = sig_of_td__loop <:ctyp@loc< $acc$ $tp$ >> tps in
      <:ctyp@loc< Typehash.t $tp$ -> $left$ >>

  let rec sig_of_tds = function
    | Ast.TyDcl (loc, name, params, _rhs, _cl) ->
      let ty = typ_constraint ~loc ~params ~name in
      let body =
        <:sig_item@loc< value $lid:body_name name$ : Typehash.internal $ty$
        >>
      in
      if params = [] then
        let base = <:sig_item@loc<
          $body$;
          value $lid:typehash_function_name name$ : Typehash.t $ty$;
          >>
        in
        if name = "t" then
          <:sig_item@loc< $base$; value typehash : Typehash.t $ty$; >>
        else
          base
      else
        body
    | <:ctyp@loc< $tp1$ and $tp2$ >> ->
      <:sig_item@loc< $sig_of_tds tp1$; $sig_of_tds tp2$ >>
    | _ -> assert false  (* impossible *)

end

let () = Pa_type_conv.add_generator_with_arg "typehash" Config.gram_entry
  (fun ctyp conf ->
   let config = match conf with None -> Config.empty | Some c -> c in
   Gen_struct.generate ~config ctyp)

let () = Pa_type_conv.add_sig_generator "typehash" Gen_sig.sig_of_tds
