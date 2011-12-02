(*pp camlp4orf *)

module List = struct
  include ListLabels
  (* import from core *)
  let init n ~f =
    if n < 0 then invalid_arg "List.init";
    let rec loop i accum =
      assert (i >= 0);
      if i = 0 then accum
      else loop (i-1) (f (i-1)::accum)
    in
    loop n []
  ;;
end


open Printf
open Camlp4.PreCast
open Pa_type_conv

module Create = struct
  let lambda _loc patterns body =
    List.fold_right patterns ~init:body ~f:(fun pattern acc ->
      <:expr< fun [ $pattern$ -> $acc$ ] >> )
  ;;

  let lambda_sig _loc arg_tys body_ty =
    List.fold_right arg_tys ~init:body_ty ~f:(fun arg_ty acc ->
      <:ctyp< $arg_ty$ -> $acc$ >> )
  ;;
end

module Variant_definition = struct
  type t = {
    name : string;
    body_ty : Ast.ctyp;
    arg_tys : Ast.ctyp list;
    kind : [ `Normal | `Polymorphic | `Type ]
  }

  let to_constructor_type t _loc =
    match t.kind with
    | `Normal | `Polymorphic -> Some (
        Create.lambda_sig _loc t.arg_tys t.body_ty
      )
    | `Type -> None
end

module Inspect = struct
  let variant _loc body_ty ty =
    let module V = Variant_definition in
    match ty with
    | <:ctyp< $uid:name$ >> -> {
        V.name = name;
        arg_tys = [];
        body_ty = body_ty;
        kind = `Normal;
      }
    | <:ctyp< $uid:name$ of $tps$ >> -> {
        V.name = name;
        arg_tys = Ast.list_of_ctyp tps [];
        kind = `Normal;
        body_ty = body_ty;
      }
    | <:ctyp< `$uid:name$ >> -> {
        V.name = name;
        arg_tys = [];
        kind = `Polymorphic;
        body_ty = body_ty;
      }
    | <:ctyp< `$uid:name$ of $tps$ >> -> {
        V.name = name;
        arg_tys = Ast.list_of_ctyp tps [];
        kind = `Polymorphic;
        body_ty = body_ty;
      }
    | <:ctyp< $lid:name$ >> -> {
        V.name = name;
        arg_tys = [];
        kind = `Type;
        body_ty = body_ty;
      }
    | <:ctyp< $lid:name$ $tps$ >> -> {
        V.name = name;
        arg_tys = Ast.list_of_ctyp tps [];
        kind = `Type;
        body_ty = body_ty;
      }
    | _ -> assert false

  let variants ty _loc body_ty = List.map (Ast.list_of_ctyp ty []) ~f:(variant _loc body_ty)

  let variant_names ty _loc body_ty = List.map (variants ty _loc body_ty)
    ~f:(fun v -> v.Variant_definition.name)
end

let raise_unsupported () =
  failwith "Unsupported use of variants (you can only use it on variant types)."

module Gen_sig = struct
  let apply_type _loc ~ty_name ~tps =
    List.fold_left tps
      ~init:<:ctyp< $lid:ty_name$ >>
      ~f:(fun acc tp -> <:ctyp< $acc$ $tp$ >>)

  let label_arg _loc name ty = Ast.TyLab (_loc, String.lowercase name, ty)
;;

  let variant_arg _loc f v =
    let variant =
      match Variant_definition.to_constructor_type v _loc with
      | None -> None
      | Some constructor_type -> (
          Some <:ctyp< Variantslib.Variant.t $constructor_type$ >>
        )
    in
    label_arg _loc v.Variant_definition.name (f ~variant)
;;

  let v_fold_fun ~ty_name ~tps _loc ty =
    let variant_type = apply_type _loc ~ty_name ~tps in
    let variants = Inspect.variants ty _loc variant_type in
    let f = variant_arg _loc (fun ~variant ->
      match variant with
      | Some variant ->
          <:ctyp< 'acc__ -> $variant$ -> 'acc__ >>
      | None -> <:ctyp< 'acc__ -> 'acc__ >>
    )
    in
    let types = List.map variants ~f in
    let init_ty = label_arg _loc "init" <:ctyp< 'acc__ >> in
    let t = Create.lambda_sig _loc
      (init_ty :: types) <:ctyp< 'acc__ >> in
    <:sig_item< value fold : $t$ >>
  ;;

  let v_iter_fun ~ty_name ~tps _loc ty =
    let variant_type = apply_type _loc ~ty_name ~tps in
    let variants = Inspect.variants ty _loc variant_type in
    let f = variant_arg _loc (fun ~variant ->
      match variant with
      | Some variant ->
          <:ctyp< $variant$ -> unit>>
      | None -> <:ctyp< unit -> unit >>
    )
    in
    let types = List.map variants ~f in
    let t = Create.lambda_sig _loc
      types <:ctyp< unit >> in
    <:sig_item< value iter : $t$ >>
  ;;

  let v_map_fun ~ty_name ~tps _loc ty =
    let module V = Variant_definition in
    let variant_type = apply_type _loc ~ty_name ~tps in
    let variants = Inspect.variants ty _loc variant_type in
    let result_type = <:ctyp< 'result__ >> in
    let f v =
      let variant =
        match V.to_constructor_type v _loc with
        | None -> <:ctyp< unit -> $result_type$ >>
        | Some constructor_type ->
            Create.lambda_sig _loc
              (<:ctyp< Variantslib.Variant.t $constructor_type$ >> ::
                  v.V.arg_tys) result_type
      in
      label_arg _loc v.V.name variant
    in
    let types = List.map variants ~f in
    let t = Create.lambda_sig _loc
      (variant_type :: types) result_type in
    <:sig_item< value map : $t$ >>
  ;;

  let v_descriptions ~ty_name:_ ~tps:_ _loc _ =
    <:sig_item< value descriptions : list (string * int)  >>

  let variant ~ty_name ~tps _loc ty =
    let variant_type = apply_type _loc ~ty_name ~tps in
    let variants = Inspect.variants ty _loc variant_type in
    let conv_variant (res_constructors,res_variants) v =
      let module V = Variant_definition in
      let constructor_type = V.to_constructor_type v _loc in
      match constructor_type with
      | None -> (res_constructors,res_variants)
      | Some constructor_type ->
          let name = String.lowercase v.V.name in
          (<:sig_item< value $lid:name$ : $constructor_type$; $res_constructors$ >>,
          <:sig_item< value $lid:name$ : Variantslib.Variant.t $constructor_type$;
           $res_variants$ >>
          )
    in
    let constructors, variants =
      List.fold_left variants ~init:(<:sig_item<>>, <:sig_item<>>) ~f:conv_variant
    in
    if ty_name = "t" then
      let fold = v_fold_fun ~ty_name ~tps _loc ty in
      let iter = v_iter_fun ~ty_name ~tps _loc ty in
      let map = v_map_fun ~ty_name ~tps _loc ty in
      let descriptions = v_descriptions ~ty_name ~tps _loc ty in
      <:sig_item< $constructors$ ;
  module Variants : sig
      $variants$;
    $fold$; $iter$; $map$; $descriptions$
  end
    >>
    else
      <:sig_item< $constructors$;
  module Variants : sig
      $variants$
  end
    >>
  ;;

  let variants_of_ty_sig _loc ~ty_name ~tps ~rhs =
    let unsupported = (fun _ _ -> raise_unsupported ()) in
    Gen.switch_tp_def
      ~alias:unsupported
      ~sum:(variant ~ty_name ~tps)
      ~record:unsupported
      ~mani:unsupported
      ~nil:(fun _ -> raise_unsupported ())
      ~variants:(variant ~ty_name ~tps)
      rhs

  let generate = function
    | Ast.TyDcl (_loc, ty_name, tps, rhs, _) -> variants_of_ty_sig _loc ~ty_name ~tps ~rhs
    | Ast.TyAnd (_loc, _, _) as tds    ->
        ignore (_loc, tds);
        failwith "Not supported"
    | _                             -> assert false
end

module Gen_struct = struct

  let variant_name_to_string v =
    match String.lowercase v with
    | "try" -> "try_"
    | s -> s

  let variants _loc ty variant_name =
    let module V = Variant_definition in
    let conv_variant (rank, res_constructors, res_variants) v =
      match v.V.kind with
      | `Type -> (rank + 1, res_constructors, res_variants)
      | (`Normal | `Polymorphic) as kind ->
          let uncapitalized = variant_name_to_string v.V.name in
          let constructor =
            let vars = List.init (List.length v.V.arg_tys) ~f:(fun i ->
              "v" ^ string_of_int i)
            in
            let constructed_value =
              let init = match kind with
                | `Normal -> <:expr< $uid:v.V.name$ >>
                | `Polymorphic -> <:expr< `$uid:v.V.name$ >>
              in
              List.fold_left vars ~init ~f:(fun acc_expr var ->
                <:expr< $acc_expr$ $lid:var$ >>)
            in
            <:str_item< value $lid:uncapitalized$ =
                $Create.lambda _loc (
                  List.map vars
                    ~f:(fun var -> <:patt< $lid:var$ >> )
                ) constructed_value $
                >>
          in
          let variant = <:str_item<
            value $lid:uncapitalized$ = (
              { Variantslib.Variant.
                  name = $str:v.V.name$;
                rank = $int:string_of_int rank$;
                constructor = $lid:uncapitalized$
              }
            )
            >>
          in
          ( rank + 1, <:str_item< $constructor$; $res_constructors$ >>,
          <:str_item< $variant$; $res_variants$ >>
          )
    in
    let variants = Inspect.variants ty _loc variant_name in
    List.fold_left variants ~init:(0, <:str_item<>>, <:str_item<>>) ~f:conv_variant
  ;;

  let label_arg ?label _loc name =
    let l =
      match label with
      | None    -> name
      | Some n  -> n in
    Ast.PaLab (_loc, l, <:patt< $lid:name$ >> )
  ;;

  let label_arg_fun _loc name =
    label_arg ~label:name _loc (name ^ "_fun__")
  ;;

  let v_fold_fun _loc ty body_ty  =
    let module V = Variant_definition in
    let variants = Inspect.variants ty _loc body_ty in
    let variant_fold acc_expr variant =
      let variant_name = variant_name_to_string variant.V.name in
      match variant.V.kind with
      | `Type ->
          <:expr< $lid:variant_name ^ "_fun__" $ $acc_expr$ >>
      | `Polymorphic | `Normal ->
          <:expr< $lid:variant_name ^ "_fun__" $ $acc_expr$ $lid:variant_name$ >>
    in
    let body =
      List.fold_left variants ~init:<:expr< init__ >> ~f:variant_fold in
    let patterns = List.map variants ~f:(fun variant ->
      label_arg_fun _loc (variant_name_to_string variant.V.name)) in
    let init = label_arg ~label:"init" _loc "init__" in
    let lambda = Create.lambda _loc
      ( init :: patterns ) body in
    <:str_item< value fold = $lambda$ >>
  ;;

  let v_descriptions _loc ty body_ty  =
    let module V = Variant_definition in
    let variants = Inspect.variants ty _loc body_ty in
    let f v =
      <:expr<($str:v.V.name$,$int:string_of_int (List.length v.V.arg_tys)$) >>
    in
    let variant_names = List.map ~f variants in
    let exList _loc = (* for some reason I can't put this at the top level *)
      let rec loop =
        function
          | [] -> <:expr< [] >>
          | x :: xs -> <:expr< [ $x$ :: $loop xs$ ] >>
      in
      loop
    in
    <:str_item< value descriptions = $exList _loc variant_names$>>
  ;;

  let v_map_fun _loc ty body_ty =
    let module V = Variant_definition in
    let variants = Inspect.variants ty _loc body_ty in
    let variant_match_case variant =
      match variant.V.kind with
      | `Type ->
          <:match_case< #$lid:variant.V.name$ ->
          $lid:variant.V.name ^ "_fun__"$ () >>
      | (`Polymorphic | `Normal) as kind ->
          let vars = List.init (List.length variant.V.arg_tys) ~f:(fun i ->
            "v" ^ string_of_int i)
          in
          let pattern =
            let init = match kind with
              | `Polymorphic ->
                  <:patt< `$uid:variant.V.name$ >>
              | `Normal ->
                  <:patt< $uid:variant.V.name$ >>
            in
            List.fold_left vars ~init ~f:(fun acc_patt var ->
              <:patt< $acc_patt$ $lid:var$ >>)
          in
          let uncapitalized = variant_name_to_string variant.V.name in
          let value = List.fold_left vars
            ~init:<:expr< $lid:uncapitalized ^ "_fun__"$ $lid:uncapitalized$ >>
            ~f:(fun acc_expr var ->
              <:expr< $acc_expr$ $lid:var$ >> )
          in
          <:match_case< $pattern$ -> $value$ >>
    in
    let body = <:expr<
      match t__ with [
        $ List.fold_left (List.tl variants) ~init:(variant_match_case (List.hd variants))
          ~f:(fun mc variant -> <:match_case< $mc$ | $variant_match_case variant$ >> )$
          ]
    >>
    in
    let patterns = List.map variants ~f:(fun variant ->
      label_arg_fun _loc (variant_name_to_string variant.V.name)) in
    let lambda = Create.lambda _loc
      ( <:patt< t__ >> :: patterns ) body in
    <:str_item< value map = $lambda$ >>
  ;;

  let v_iter_fun _loc ty body_ty  =
    let module V = Variant_definition in
    let variants = Inspect.variants ty _loc body_ty in
    let names = List.map variants ~f:(fun v -> variant_name_to_string v.V.name) in
    let variant_iter variant =
      let variant_name = variant_name_to_string variant.V.name in
      let apply_to =
        match variant.V.kind with
        | `Type -> <:expr< () >>
        | `Polymorphic | `Normal -> <:expr< $lid:variant_name$ >>
      in
      <:expr< $lid:variant_name ^ "_fun__" $ $apply_to$ >> in
    let body =
      List.fold_left (List.tl variants)
        ~init:(variant_iter (List.hd variants))
        ~f:(fun acc n -> <:expr< ( $acc$; $variant_iter n$ ) >>) in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let lambda = Create.lambda _loc
      ( patterns ) body in
    <:str_item< value iter = $lambda$ >>
  ;;

  let variant ~variant_name ~tps _loc ty =
    let body_ty = Create.lambda_sig _loc tps <:ctyp< $lid:variant_name$>> in
    let _num_variants, constructors, variants = variants _loc ty body_ty in
    if variant_name = "t" then
      let fold = v_fold_fun _loc ty body_ty in
      let iter = v_iter_fun _loc ty body_ty in
      let map = v_map_fun _loc ty body_ty in
      let descriptions = v_descriptions _loc ty body_ty in
      <:str_item<
        $constructors$;
  module Variants = struct
      $variants$;
    $fold$; $iter$; $map$;
    $descriptions$
  end >>
    else <:str_item<
      $constructors$;
  module Variants = struct
      $variants$
  end>>
  ;;

  let mani ~variant_name ~tps _loc _ ty =
    match ty with
    | <:ctyp< [ $x$ ] >> ->
      variant ~variant_name ~tps _loc x
    | _ -> failwith "the right hand side of the manifest must be a record"

  let variants_of_ty _loc ~variant_name ~tps ~rhs =
    let unsupported = (fun _ _ -> raise_unsupported ()) in
    Gen.switch_tp_def
      ~alias:unsupported
      ~sum:(variant ~variant_name ~tps)
      ~variants:(variant ~variant_name ~tps)
      ~mani:(mani ~variant_name ~tps)
      ~nil:(fun _ -> raise_unsupported ())
      ~record:unsupported
      rhs

  let generate = function
    | Ast.TyDcl (_loc, name, tps, rhs, _) -> variants_of_ty _loc ~variant_name:name ~tps ~rhs
    | Ast.TyAnd (_loc, _, _) as tds ->
        ignore (_loc, tds);
        failwith "Not supported"
    | _                             -> assert false
end

let _ = add_generator "variants" Gen_struct.generate
let _ = add_sig_generator "variants" Gen_sig.generate
