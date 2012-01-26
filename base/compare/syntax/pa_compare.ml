
(* Note: I am introducing a few unnecessary explicit closures,
  (not all of them some are unnecessary due to the value restriction).
*)

open StdLabels
open Printf
open Camlp4.PreCast
module Gen = Pa_type_conv.Gen

let (|!) x f = f x

module Pa_tools : sig
  val with_tuple : Ast.Loc.t -> value:Ast.expr -> ty:Ast.ctyp ->
    ((Ast.expr * Ast.ctyp) list -> Ast.expr) -> Ast.expr
  val with_record : Ast.Loc.t -> value:Ast.expr -> ty:Ast.ctyp ->
    ((Ast.expr * Ast.ctyp) list -> Ast.expr) -> Ast.expr
  val unpack_ty_app : Ast.ctyp -> Ast.ident * string list * Ast.ctyp list
end = struct


  let with_tuple _loc ~value ~ty f =
    (* generate
       let id_1, id_2, id_3, ... id_n = value in expr
       where expr is the result of (f [id_1, ty_1 ; id_2, ty_2; ...])
     *)
    let names_types = List.map (Ast.list_of_ctyp ty [])
      ~f:(fun t -> Gen.gensym ~prefix:"t" (), t) in
    let pattern =
      let l = List.map names_types ~f:(fun (n, _) -> <:patt< $lid:n$ >>) in
      <:patt< ( $tup:Ast.paCom_of_list l$ ) >>
    in
    let e        = f (List.map names_types ~f:(fun (n,t) -> (<:expr< $lid:n$ >>, t))) in
    let binding  = <:binding< $pattern$ = $value$ >> in
    <:expr< let $binding$ in $e$ >>

  let eval_expr_once _loc e f =
    match e with
    | <:expr< $id:_$ >> -> f e
    | _ ->
      let n = Gen.gensym () in
      <:expr< let $lid:n$ = $e$ in $f <:expr< $lid:n$ >> $ >>

  let with_record _loc ~value ~ty f =
    (* generate
        let r = value in expr where expr is the result of
        f [ r.fieldname1, fieldtype1; ... ]
     *)
    let l = List.map (Ast.list_of_ctyp ty []) ~f:(function
       | <:ctyp< $lid:name$ : mutable $field_ty$ >> -> (name, field_ty)
       | <:ctyp< $lid:name$ : $field_ty$ >>         -> (name, field_ty)
       | _                                          -> assert false)
    in
    eval_expr_once _loc value (fun record ->
      f (List.map l ~f:( fun (n, t) -> ( <:expr< $record$.$lid:n$ >> ,t))))

  let unpack_ty_app ty =
    let rec loop t acc =
      match t with
      | Ast.TyApp (_, t, a) -> loop t (a :: acc)
      | Ast.TyId  (_, id)   -> id, Gen.get_rev_id_path id [], List.rev acc
      | _               -> assert false
    in
    loop ty []
end

let bind_pats = function
  | [v] -> v
  | l ->
    let v = Ast.paCom_of_list l in
    let loc = Ast.loc_of_patt v in
    <:patt@loc< ( $tup:v$ ) >>

let rec tds_contains_t = function
  | Ast.TyDcl (_, "t", _, _, _) -> true
  | <:ctyp< $t1$ and $t2$ >> -> tds_contains_t t1 || tds_contains_t t2
  | _ -> false

module Gen_struct = struct

   let phys_equal_first a b cmp =
     let loc = Ast.loc_of_expr cmp in
     <:expr@loc< if Pervasives.(==) $a$ $b$ then 0 else $cmp$ >>

   let rec chain_if  = function
     | [] -> assert false
     | [x] -> x
     | x :: xs ->
       let loc = Ast.loc_of_expr x in
       <:expr@loc< let ret = $x$ in if ret <> 0 then ret else $chain_if xs$ >>

  let base_types =
    [ "nativeint"; "int64"; "int32"; "char"; "int"; "bool"; "string"; "float" ]

  let compare_named name =
    let loc = Ast.loc_of_ident name in
    match Gen.get_rev_id_path name [] with
    | [ v ] when List.mem ~set:base_types v ->
      <:expr@loc< Pervasives.compare >>
    | ["unit"]         -> <:expr@loc< fun _ _ -> 0 >>
    | "t" :: path ->
      <:expr@loc< $id:Gen.ident_of_rev_path loc ("compare" :: path)$ >>
    | tn :: path ->
      <:expr@loc< $id:Gen.ident_of_rev_path loc (("compare_" ^ tn) :: path)$ >>
    | [] -> assert false

  let tp_name n = sprintf "_cmp__%s" n

  let rec compare_applied ty value1 value2 =
    let _loc = Ast.loc_of_ctyp ty in
    match Pa_tools.unpack_ty_app ty with
    | _, ["ref"], [t] ->
        let e1 = <:expr< $value1$.Pervasives.contents >> in
        let e2 = <:expr< $value2$.Pervasives.contents >> in
        compare_of_ty t e1 e2
    | _, ["option"], [t] ->
        let a = Gen.gensym () in
        let b = Gen.gensym () in
        <:expr<
          match ($value1$, $value2$) with [
            (None, None)   -> 0
          | (None, Some _) -> 1
          | (Some _, None) -> -1
          | (Some $lid:a$, Some $lid:b$) ->
            $compare_of_ty t <:expr< $lid:a$ >> <:expr< $lid:b$ >> $
        ]
        >>
    | _, ["array"], [t] ->
      compare_array t value1 value2
    | _,["list"], [t] ->
      compare_list t value1 value2
    | name, _, ta ->
      let args = List.map ta ~f:compare_of_ty_fun in
      let cmp = Gen.apply _loc (compare_named name) args in
      <:expr< $cmp$ ($value1$:$ty$) ($value2$:$ty$) >>

  and compare_list t value1 value2 =
    let loc = Ast.loc_of_ctyp t in
    <:expr@loc<
      let rec loop a b =
        match (a, b) with
        [ ([], []) -> 0
        | ([], _) -> (-1)
        | (_, []) -> 1
        | ([x :: xs], [y :: ys]) ->
          let n = $compare_of_ty t <:expr@loc< x >> <:expr@loc< y >>$
          in if n = 0 then loop xs ys else n ]
      in loop $value1$ $value2$ >>

  and compare_array t value1 value2 =
    let loc = Ast.loc_of_ctyp t in
    <:expr@loc<
      if Pervasives.(==) $value1$ $value2$ then
        0
      else
        let len_a = Array.length $value1$ in
        let len_b = Array.length $value2$ in
        let ret = Pervasives.compare len_a len_b in
        if ret <> 0 then ret
        else
          let rec loop i =
            if i = len_a then
              0
            else
              let l = Array.unsafe_get $value1$ i
              and r = Array.unsafe_get $value2$ i in
              let res = $compare_of_ty t <:expr@loc< l >> <:expr@loc< r >>$ in
              if res <> 0 then res
              else loop (i+1)
          in
          loop 0
        >>


  and compare_of_tuple t value1 value2 =
    let _loc = Ast.loc_of_ctyp t in
    Pa_tools.with_tuple _loc ~value:value1 ~ty:t (fun elems1 ->
    Pa_tools.with_tuple _loc ~value:value2 ~ty:t (fun elems2 ->
    let exprs = List.map2 elems1 elems2 ~f:(fun (v1, t) (v2, _) ->
      compare_of_ty t v1 v2)
    in
    chain_if exprs ))

  and compare_variant ty value1 value2 =
    let rec loop = function
      | <:ctyp< $tp1$ | $tp2$ >> -> loop tp1 @ loop tp2
      | <:ctyp@loc< `$cnstr$ >> ->
         [ <:match_case@loc< ( `$cnstr$ , `$cnstr$ ) -> 0 >> ]
      | <:ctyp@loc< `$cnstr$ of $tp$ >> ->
          let v1 = Gen.gensym ~prefix:"_left" ()
          and v2 = Gen.gensym ~prefix:"_right" () in
          let body = compare_of_ty tp
            <:expr@loc< $lid:v1$ >>
            <:expr@loc< $lid:v2$ >>
          in
          [ <:match_case@loc< (`$cnstr$ $lid:v1$, `$cnstr$ $lid:v2$)
            -> $body$ >> ]
      | <:ctyp< [= $row_fields$ ] >>  | <:ctyp< [< $row_fields$ ] >>
        -> loop row_fields
      | <:ctyp@loc< $_$ $id:id$ >> as ty ->
          let v1 = Gen.gensym ~prefix:"_left" ()
          and v2 = Gen.gensym ~prefix:"_right" () in
          let call = compare_applied ty
            <:expr@loc< $lid:v1$>>
            <:expr@loc< $lid:v2$>>
          in
          [ <:match_case@loc< ((#$id$ as $lid:v1$),(#$id$ as $lid:v2$)) ->
          $call$ $lid:v1$ $lid:v2$ >> ]
      | <:ctyp@loc< $id:id$ >> ->
          let call = compare_named id in
          let v1 = Gen.gensym ~prefix:"_left" ()
          and v2 = Gen.gensym ~prefix:"_right" () in
          [ <:match_case@loc< ((#$id$ as $lid:v1$),(#$id$ as $lid:v2$)) ->
          $call$ $lid:v1$ $lid:v2$ >> ]
      | <:ctyp< [> $_$ ] >> | <:ctyp< [< $_$ > $_$ ] >>  as ty ->
        Gen.error ty
          ~fn:"compare_variant"
          ~msg:"cannot compare open polymorphic variant types"
      | tp -> Gen.unknown_type tp "compare_variant"
    in
    let loc = Ast.loc_of_ctyp ty in
    let e = match loop ty with
      | [ v ] -> <:expr@loc< match ( $value1$,$value2$ ) with [ $v$ ] >>
        | l     ->
          <:expr@loc< match ( $value1$,$value2$ ) with
            [ $Ast.mcOr_of_list l$
      (* Providing we didn't screw up badly we now know that the tags of the
         variants are different. We let pervasive do its magic. *)
            | (x,y) -> Pervasives.compare x y ] >>
    in
    phys_equal_first value1 value2 e

  and branches_of_sum ~rightmost= function
    | <:ctyp@loc< $uid:id$  >> when rightmost ->
      <:match_case@loc< ($uid:id$,$uid:id$) -> 0 >>
    | <:ctyp@loc< $uid:id$  >> ->
      <:match_case@loc< ($uid:id$,$uid:id$) -> 0
      | ($uid:id$,_) -> (-1)
      | (_,$uid:id$) -> 1
        >>
    | <:ctyp@loc< $uid:id$ of $tps$ >> ->
      let ids_ty = List.map (Ast.list_of_ctyp tps [])
        ~f:(fun ty ->
          Gen.gensym ~prefix:"_a" (),
          Gen.gensym ~prefix:"_b" (),
          ty)
      in
      let lpatt = List.map ids_ty
        ~f:(fun (l,_r,ty) -> <:patt@loc< ( $lid:l$ : $ty$ ) >>)
                  |! bind_pats
      and rpatt = List.map ids_ty
        ~f:(fun (_l,r,ty) -> <:patt@loc< ( $lid:r$ : $ty$ ) >>)
                  |! bind_pats
      and body = List.map ids_ty
        ~f:(fun (l,r,ty) ->
          compare_of_ty ty <:expr@loc< $lid:l$ >> <:expr@loc< $lid:r$ >>)
        |! chain_if
      in
      let res =
        <:match_case@loc< (($uid:id$ $lpatt$),($uid:id$ $rpatt$)) -> $body$ >>
      in
      if rightmost then
        res
      else
        <:match_case@loc< $res$
         | ($uid:id$ _,_) -> (-1)
         | (_,$uid:id$ _) -> 1
        >>
    | <:ctyp@loc< $tp1$ | $tp2$ >> ->
      <:match_case@loc< $branches_of_sum ~rightmost:false tp1$
              | $branches_of_sum ~rightmost tp2$ >>
    | _ -> assert false

  and compare_sum ctype value1 value2 =
    let loc = Ast.loc_of_ctyp ctype in
    let mc = branches_of_sum ~rightmost:true ctype in
    let e =  <:expr@loc< match ($value1$,$value2$) with [ $mc$ ] >> in
    phys_equal_first value1 value2 e

  and compare_of_ty ty value1 value2 =
    match ty with
    | <:ctyp@loc< $id:id$ >> ->
        <:expr@loc<$compare_named id$ ($value1$:$ty$) ($value2$:$ty$) >>
    | <:ctyp< $_$ $_$ >> -> compare_applied ty value1 value2
    | <:ctyp< $tup:t$ >> -> compare_of_tuple t value1 value2
    | <:ctyp@loc< '$name$ >> ->
      <:expr@loc< $lid:tp_name name$ $value1$ $value2$ >>
    | <:ctyp< $_$ -> $_$ >> ->
      Gen.error ty ~fn:"compare_of_ty" ~msg:"Functions can not be compared."
    | <:ctyp< [= $variants$ ] >> ->
      compare_variant variants value1 value2
    | ty -> Gen.unknown_type ty "compare_of_ty"

  and compare_of_ty_fun ty =
    let _loc = Ast.loc_of_ctyp ty in
    let a = Gen.gensym ~prefix:"a" () in
    let b = Gen.gensym ~prefix:"b" () in
    <:expr< fun ( $lid:a$ : $ty$) ( $lid:b$ : $ty$) ->
      $compare_of_ty ty <:expr< $lid:a$ >> <:expr< $lid:b$ >> $ >>

  let compare_of_record ctype value1 value2 =
      let _loc = Ast.loc_of_ctyp ctype in
      let expr =
        Pa_tools.with_record _loc ~value:value1 ~ty:ctype (fun fields1 ->
        Pa_tools.with_record _loc ~value:value2 ~ty:ctype (fun fields2 ->
          let exprs = List.map2 fields1 fields2 ~f:(fun (v1,t) (v2,_) ->
            compare_of_ty t v1 v2)
          in
          chain_if exprs))
      in
      phys_equal_first value1 value2 expr

  let compare_of_td loc type_name tps rhs =
    let a = Gen.gensym ~prefix:"a" () in
    let b = Gen.gensym ~prefix:"b" () in
    let v_a = <:expr@loc< $lid:a$ >> in
    let v_b = <:expr@loc< $lid:b$ >> in
    let body =
      let rec loop tp =
        Gen.switch_tp_def
          ~alias:(fun _loc ty -> compare_of_ty ty v_a v_b)
          ~sum:(fun _loc ty -> compare_sum ty v_a v_b)
          ~variants:(fun _loc ty -> compare_variant ty v_a v_b)
          ~mani:(fun (_:Loc.t) _tp1 tp2 -> loop tp2)
          ~nil:(fun _ -> assert false)
          ~record:(fun _loc ty -> compare_of_record ty v_a v_b)
          tp
      in
      loop rhs
    in
    let extra_names = List.map tps
      ~f:(fun t -> tp_name (Gen.get_tparam_id t))
    in
    let patts =
      List.map (extra_names @ [a; b])
        ~f:(fun v -> <:patt@loc< $lid:v$ >>)
    and bnd = <:patt@loc< $lid:if type_name = "t" then
        "compare"
      else
        "compare_" ^ type_name
    $ >> in
    <:binding@loc< $bnd$ = $Gen.abstract loc patts body$ >>

  let rec compare_of_tds = function
     | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
       compare_of_td loc type_name tps rhs
     | <:ctyp@loc< $tp1$ and $tp2$ >> ->
       <:binding@loc< $compare_of_tds tp1$ and $compare_of_tds tp2$ >>
     | _ -> assert false  (* impossible *)

  let compare_of tds =
    let binding, recursive,loc =
      match tds with
      | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
          compare_of_td loc type_name tps rhs,
          Gen.type_is_recursive type_name rhs, loc
      | <:ctyp@loc< $_$ and $_$ >> as tds -> compare_of_tds tds, true, loc
      | _ -> assert false  (* impossible *)
    in
    let body =
      if recursive then <:str_item@loc< value rec $binding$ >>
      else <:str_item@loc< value $binding$ >>
    in
    if tds_contains_t tds then
      <:str_item@loc< $body$ ; value compare_t = compare >>
    else
      body
end

module Gen_sig = struct

  let rec sig_of_td__loop typ = function
    | [] ->
      let loc = Ast.loc_of_ctyp typ in
      <:ctyp@loc< $typ$ -> $typ$ -> int >>
    | tp :: tps ->
      let tp = Gen.drop_variance_annotations tp in
      let loc = Ast.loc_of_ctyp tp in
      let compare_of = sig_of_td__loop <:ctyp@loc< $typ$ $tp$ >> tps in
      <:ctyp@loc< ( $tp$  -> int ) -> $compare_of$ >>

  let sig_of_td loc type_name tps _rhs _cl =
    let compare_of = sig_of_td__loop <:ctyp@loc< $lid:type_name$ >> tps in
    let name = match type_name with
      | "t" -> "compare"
      | _ -> "compare_" ^ type_name
    in
    <:sig_item@loc< value $lid: name$ : $compare_of$ >>

  let rec sig_of_tds = function
    | Ast.TyDcl (loc, type_name, tps, rhs, cl) ->
      sig_of_td loc type_name tps rhs cl
    | <:ctyp@loc< $tp1$ and $tp2$ >> ->
      <:sig_item@loc< $sig_of_tds tp1$; $sig_of_tds tp2$ >>
    | _ -> assert false  (* impossible *)
end

module Gen_quote = struct
  let parse loc _loc_name_opt cnt_str =
    Pa_type_conv.set_conv_path_if_not_set loc;
    let ctyp = Gram.parse_string Syntax.ctyp_quot loc cnt_str in
    Gen_struct.compare_of_ty_fun ctyp
end

let () =
  Syntax.Quotation.add "compare" Syntax.Quotation.DynAst.expr_tag
    Gen_quote.parse

let () = Pa_type_conv.add_sig_generator "compare" Gen_sig.sig_of_tds
let () = Pa_type_conv.add_generator "compare" Gen_struct.compare_of
