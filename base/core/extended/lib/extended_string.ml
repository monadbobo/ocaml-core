open Core.Std;;

(* Natural ordering like found in gnome nautilus, the mac finder etc...
   Refer to Mli for more documentation
*)
let collate s1 s2 =
  let pos1 = ref 0
  and pos2 = ref 0  in

  let next ~ok s pos =
    if (!pos) = String.length s then
      None
    else
      let c = s.[!pos] in
      if ok c then begin
        incr pos;
        Some c
      end else
        None
  in

  let compare_non_numerical () =
    let ok c = not (Char.is_digit c) in
    let rec loop () =
      match next ~ok s1 pos1,next ~ok s2 pos2 with
      | Some _, None -> 1
      | None , Some _ -> -1
      | None , None -> 0
      | Some c1,Some c2 when c1 = c2 -> loop ()
      | Some c1,Some c2 -> Char.compare c1 c2
    in
    loop ()
  in

  let compare_numerical () =
    let rec consume0 s pos  =
      match next ~ok:((=) '0') s pos with
      | Some _ -> consume0 s pos
      | None ->  ()
    in
     (* Our main loop works on string representation of ints where all the
        trailing zeros have been chopped of. Their magnitude is given by the
        length of their representation. If they have the same magnitude the
        lexical order is correct. Bias is used to save that information.
     *)
    let ok = Char.is_digit in
    let bias = ref 0 in
    let rec loop () =
      match next ~ok s1 pos1,next ~ok s2 pos2 with
      | Some _, None -> 1
      | None , Some _ -> -1
      | None , None when !bias <> 0-> !bias
      | None , None ->
          (* Both ints have the same value, The one with the shortest
             representation (i.e. the least trailing zeroes) is
             considered to be the smallest*)
          !pos1 - !pos2
      | Some c1,Some c2 when !bias = 0 ->
          bias := Char.compare c1 c2;
          loop ()
      | Some _ , Some _ -> loop ()
    in
    consume0 s1 pos1;
    consume0 s2 pos2;
    loop ()
  in

  let s1_length = String.length s1 in
  let s2_length = String.length s2 in
  let rec loop () =
    let r  = compare_non_numerical () in
    let r' = compare_numerical () in
    match r,r' with
    | 0,0 when !pos1 = s1_length && !pos2 = s2_length -> 0
    | 0,0 -> loop ()
    | 0,i | i,_ -> i
  in
  loop ()
;;

TEST_MODULE "collate" = struct
  let (<!) s s' = collate s s' < 0
  and (>!) s s' = collate s s' > 0

  (* let basic_tests = (fun (s,s') ->
    "invertible" @? ((s' <! s) = (s >! s'));
    "total" @? (definitive_clause [s<!s'; s=s'; s>!s'])) *)

  (* repeat 50 basic_tests (pg sg sg);
  repeat 2 basic_tests (dup sg);
  repeat 50 (fun (s,s',s'') ->
    let (s1,s2,s3) =
      match List.sort ~cmp:String.collate [s;s';s''] with
      | [s1;s2;s3] -> s1,s2,s3
      | _ -> assert false
    in
    "transitive" @?
      (((s1 <! s2) || (s2 <! s3)) = (s1 <! s3)))
    (tg sg sg sg); *)

  TEST = "a2b" <! "a10b"
  TEST = "a2b" <! "a02b"
  TEST = "a010b" <! "a20b"

end

let concat_map ?sep ~f l =
  String.concat ?sep (List.map ~f l)

(**
   Inverse operation of [String.escaped]
*)
exception Unescape_error of bool*int*string

(* The stdlib's escaped does a lot of fancy wazoo magic to avoid
   using a buffer:
   It works in two passes, the first one calculates the length of the string to
   allocate and the second one does the actual escaping.

   This would be more cumbersome to do here but might be worth the hassle if
   performance ever gets to be an issue *)
let unescaped' ?(strict=true) s =
  let len = String.length s in
  let pos = ref 0 in
  let error ?(fatal=false) message =
    raise (Unescape_error (fatal,!pos,message))
  in
  let consume () =
    let i = !pos in
    if i = len then error "unexpectedly reached end of string";
    let c = s.[i] in
    pos := i + 1;
    c
  in
  let res = Buffer.create len in
  let emit c = Buffer.add_char res c in
  let emit_code code =
    match Char.of_int code with
    | Some c -> emit c
    | None -> error ~fatal:true
        (Printf.sprintf "got invalid escape code %d" code)
  in
  let rec loop () =
    if !pos < len then begin
      let c = consume () in
      if c <> '\\' then
        emit c
      else begin
        let mark = !pos in
        try
          let c = consume () in
          match c with
          | '\\' | '\"' -> emit c
          | 'b' -> emit '\b'
          | 'n' -> emit '\n'
          | 'r' -> emit '\r'
          | 't' -> emit '\t'
          | '\n' ->
              let rec consume_blank () =
                if !pos < len then begin
                  match consume () with
                  | ' ' | '\t' -> consume_blank ()
                  | _ -> decr pos
                end
              in
              consume_blank ()
          | 'x' ->
              let c2hex c =
                if (c >= 'A') && (c <= 'F' ) then
                  (Char.to_int c) + 10 - Char.to_int 'A'
                else if (c >= 'a') && (c <= 'f' ) then
                  (Char.to_int c) + 10 - Char.to_int 'a'
                else if (c >= '0') && (c <= '9') then
                  (Char.to_int c) - Char.to_int '0'
                else
                  error (Printf.sprintf "expected hex digit, got: %c" c);
              in
              let c1 = consume () in
              let c2 = consume () in
              emit_code (16 * c2hex c1 + c2hex c2);
          | c when Char.is_digit c ->
              let char_to_num c =
                match Char.get_digit c with
                | None -> error (Printf.sprintf "expected digit,got: %c" c);
                | Some i -> i
              in
              let i1 = char_to_num c in
              let i2 = char_to_num (consume ()) in
              let i3 = char_to_num (consume ()) in
              emit_code (100 * i1 + 10 * i2 + i3);
          | c -> error (Printf.sprintf "got invalid escape character: %c" c);
        with Unescape_error (false,_,_) when not strict ->
          emit '\\';
          pos := mark
      end;
      loop ()
    end else
      Buffer.contents res;
  in
  loop ();
;;

let unescaped ?strict s =
  try
    unescaped' ?strict s
  with Unescape_error (_,pos,message) ->
    invalid_argf "String.unescaped error at position %d of %s: %s"
      pos s message ()

let unescaped_res ?strict s =
  try
    Core.Result.Ok (unescaped' ?strict s)
  with Unescape_error (_,pos,message) ->
    Core.Result.Error (pos,message)


let squeeze str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec skip_spaces i =
    if i >= len then
      Buffer.contents buf
    else
      let c = str.[i] in
      if (c = ' ') || (c = '\n') || (c = '\t') || (c = '\r') then
        skip_spaces (i+1)
      else
        begin
          Buffer.add_char buf c;
          copy_chars (i+1)
        end
  and copy_chars i =
    if i >= len then
      Buffer.contents buf
    else
      let c = str.[i] in
      if (c = ' ') || (c = '\n') || (c = '\t') || (c = '\r') then
        begin
          Buffer.add_char buf ' ';
          skip_spaces (i+1)
        end
      else
        begin
          Buffer.add_char buf c;
          copy_chars (i+1)
        end
  in
  copy_chars 0
;;


let pad_right ?(char=' ') s l =
  let src_len = String.length s in
  if src_len >= l then
    s
  else
    let res = String.create l in
    String.blit ~src:s ~dst:res ~src_pos:0 ~dst_pos:0 ~len:src_len;
    String.fill ~pos:src_len ~len:(l-src_len) res char;
    res

let pad_left ?(char=' ') s l =
  let src_len = String.length s in
  if src_len >= l then
    s
  else
    let res = String.create l in
    String.blit ~src:s ~dst:res ~src_pos:0 ~dst_pos:(l-src_len) ~len:src_len;
    String.fill ~pos:0 ~len:(l-src_len) res char;
    res

let line_break ~len s =
  let buf = Buffer.create len in
  let flush_buf () =
    let res = Buffer.contents buf in
    Buffer.reset buf;
    res
  in
  let rec loop acc = function
    | [] ->
        let acc = if Buffer.length buf <> 0 then
          flush_buf ():: acc
        else if acc = [] then
          [""]
        else
          acc
        in
        List.rev acc
    | h::t when Buffer.length buf = 0 ->
        Buffer.add_string buf h;
        loop acc t
    | h::t when (Buffer.length buf + 1 + String.length h) < len ->
        Buffer.add_char buf ' ';
        Buffer.add_string buf h;
        loop acc t
    | l ->
        loop (flush_buf ()::acc) l
  in
  List.concat_map (String.split ~on:'\n' s)
    ~f:(fun s -> loop [] (String.split ~on:' ' s))

(* Finds out where to break a given line; returns the len of the line to break
    and the staring position of the next line.*)
let rec word_wrap__break_one
    ~hard_limit
    ~soft_limit
    ~previous_match
    s
    ~pos
    ~len =
  if pos = String.length s then
    len,pos
  else if previous_match > 0 && len >= soft_limit then
    previous_match,pos-len+previous_match+1
  else if len >= hard_limit then
    len,pos
  else
    match s.[pos] with
      (* Detect \r\n as one newline and not two... *)
    | '\r' when pos < String.length s -1 && s.[pos + 1] = '\n' ->
        len,pos+2
    | '\r' | '\n' -> len,pos+1
    | ' ' | '\t' ->
        word_wrap__break_one s
          ~hard_limit
          ~soft_limit
          ~previous_match:len
          ~pos:(pos+1)
          ~len:(len+1)
    | _ ->
        word_wrap__break_one s
          ~previous_match
          ~hard_limit
          ~soft_limit
          ~pos:(pos+1)
          ~len:(len+1)


(* Returns an pos*length list of all the lines (as substrings of the argument
   passed in) *)
let rec word_wrap__find_substrings ~hard_limit ~soft_limit s acc pos =
  if pos < String.length s then begin
    let len,new_pos = word_wrap__break_one s
      ~hard_limit
      ~soft_limit
      ~previous_match:0
      ~pos
      ~len:0
    in
    word_wrap__find_substrings
      ~hard_limit
      ~soft_limit
      s
      ((pos,len)::acc)
      new_pos
  end else
    acc

let word_wrap
    ?(trailing_nl=false)
    ?(soft_limit=80)
    ?(hard_limit=Int.max_value)
    ?(nl="\n") s =
  let soft_limit = min soft_limit hard_limit in
  let lines = word_wrap__find_substrings ~soft_limit ~hard_limit s [] 0 in
  match lines with
  | [] | [_] ->
      if trailing_nl then s^nl else s
  | ((hpos,hlen)::t) ->
      let nl_len = String.length nl in
      let body_len =
        List.fold_left t
          ~f:(fun acc (_,len) -> acc + nl_len + len)
          ~init:0
      in
      let res_len =
        if trailing_nl then body_len+hlen+nl_len else body_len+hlen
      in
      let res = String.create res_len in
      if trailing_nl then begin
        String.blit
          ~src:nl
          ~dst:res
          ~len:nl_len
          ~src_pos:0
          ~dst_pos:(body_len+hlen);
      end;
      String.blit
        ~src:s
        ~dst:res
        ~len:hlen
        ~src_pos:hpos
        ~dst_pos:body_len;
      let rec blit_loop dst_end_pos = function
        | [] -> ()
        | (src_pos,len)::rest ->
            let dst_pos = dst_end_pos-len-nl_len in
            String.blit
              ~src:s
              ~dst:res
              ~len
              ~src_pos
              ~dst_pos;
            String.blit
              ~src:nl
              ~dst:res
              ~len:nl_len
              ~src_pos:0
              ~dst_pos:(dst_pos + len);
            blit_loop dst_pos rest
      in
      blit_loop body_len t;
      res

(* Knuth-Morris-Pratt string matching. *)
let is_substring  ~substring t =
  let kmp_prefix len ~substring =
    let prefix = Array.create len 0 in
    let rec f ~k ~q =
      if q > len then prefix
      else (
        let k =
          let rec g k =
            if k <= 0 || ((String.get substring k) = (String.get substring (q - 1))) then k
            else g (prefix.(k - 1))
          in
          g k
        in
        let k =
          if (String.get substring k) = (String.get substring (q - 1))
          then k + 1 else k
        in
        assert (q - 1 >= 0 && q - 1 < len);
        Array.set prefix (q - 1) k;
        f ~k ~q:(q + 1)
      )
    in
    f ~k:0 ~q:2
  in
  let n = String.length t in
  let m = String.length substring in
  let prefix = kmp_prefix m ~substring in
  let rec f ~q ~i =
    if i > n then false
    else
    let q = (
      let q =
        let rec g q =
          if q <= 0 || ((String.get substring q) = (String.get t (i - 1)))
          then q
          else g (prefix.(q - 1))
        in
        g q
      in
      if String.get substring q = String.get t (i - 1) then q + 1
      else q
    )
    in
    if q = m then true
    else f ~q ~i:(i + 1)
  in
  f ~q:0 ~i:1

module Escaping = struct
  let escape_gen ~escapeworthy_map ~escape_char s =
    let get_code_for c =
      if c = escape_char then c else
        match List.Assoc.find escapeworthy_map c with
        | None -> failwith "escape_gen bug"
        | Some x -> x
    in
    let escapeworthy c =
      c = escape_char || List.exists ~f:(fun (c',_) -> c = c') escapeworthy_map in
    let count_escapeworthy_chars s =
      let ctr = ref 0 in
      for i = 0 to String.length s - 1 do
        if escapeworthy s.[i] then ctr := !ctr + 1
      done;
      !ctr
    in
    let really_escape_string count s =
      let ns = String.create count in
      let ns_pos = ref 0 in
      for i = 0 to String.length s - 1 do
        if escapeworthy s.[i] then
          begin
            ns.[!ns_pos] <- escape_char;
            ns.[!ns_pos + 1] <- get_code_for s.[i];
            ns_pos := !ns_pos + 2
          end
        else
          begin
            ns.[!ns_pos] <- s.[i];
            ns_pos := !ns_pos + 1
          end
      done;
      ns
    in
    let ewc = count_escapeworthy_chars s in
    if ewc > 0 then really_escape_string (ewc + String.length s) s
    else s

  TEST_MODULE "escape_gen" = struct
    let escape = escape_gen
      ~escapeworthy_map:[('%','p');('^','c')] ~escape_char:'_'

    TEST = escape "foo" = "foo"
    TEST = escape "_" = "__"
    TEST = escape "foo%bar" = "foo_pbar"
    TEST = escape "^foo%" = "_cfoo_p"
  end


  let escape ~escapeworthy ~escape_char str =
    let escapeworthy = escape_char :: escapeworthy in
    let threshold = List.fold ~f:(fun m p -> if p < fst m then (p, snd m)
                                       else if p > snd m then (fst m, p) else m)
      ~init:(List.hd_exn escapeworthy, List.hd_exn escapeworthy) escapeworthy in
    let min = fst threshold in
    let max = snd threshold in
    let strlen = (String.length str) - 1 in
    let smark = ref 0
    and dmark = ref 0 in
    let newstr =  String.create ((strlen + 1) * 2) in
    let blit send =   (* copies smark to send into newstr *)
      let len = (send - !smark + 1) in
      String.blit ~src:str ~src_pos:!smark ~dst:newstr ~dst_pos:!dmark ~len:len;
      smark := send + 1;
      dmark := !dmark + len;
    in
    let rec exists lst c =
      match lst with
      | [] -> false
      | p :: rst -> if p = c then true else exists rst c
    in
    for i = 0 to strlen do
      if str.[i] >= min && str.[i] <= max &&
        List.exists ~f:(fun c -> c = str.[i]) escapeworthy then
          begin
            blit i;
            newstr.[!dmark - 1] <- escape_char;
            newstr.[!dmark] <- str.[i];
            dmark := !dmark + 1;
          end
    done;
    if !smark > 0 then
      begin
        if !smark <= strlen then blit strlen;
        String.sub newstr ~pos:0 ~len:!dmark
      end
    else
      str

   TEST_MODULE "escape" = struct
     let escape = escape
       ~escapeworthy:['%';'^'] ~escape_char:'_'

     TEST = escape "foo" = "foo"
     TEST = escape "_" = "__"
     TEST = escape "foo%bar" = "foo_%bar"
     TEST = escape "^foo%" = "_^foo_%"

   end

  let escape_one_orig ~escapeworthy ~escape_char str =
    let len = String.length str in
    let rec loop cnt i =
      if i < 0 then cnt
      else
        let new_i = i - 1 in
        let c = str.[i] in
        if c = escapeworthy || c = escape_char then loop (cnt + 2) new_i
        else loop (cnt + 1) new_i
    in
    let len_1 = len - 1 in
    let cnt = loop 0 len_1 in
    if cnt = len then str
    else
      let res = String.create cnt in
      let rec loop src_pos dst_pos =
        if src_pos < 0 then res
        else
          let new_src_pos = src_pos - 1 in
          let c = str.[src_pos] in
          res.[dst_pos] <- c;
          if c = escapeworthy || c = escape_char then (
            res.[dst_pos - 1] <- escape_char;
            loop new_src_pos (dst_pos - 2))
          else loop new_src_pos (dst_pos - 1)
      in
      loop len_1 (cnt - 1)

    TEST_MODULE "escape_one_orig" = struct
      let escape = escape_one_orig
        ~escapeworthy:'%' ~escape_char:'_'

      TEST = escape "foo" = "foo"
      TEST = escape "_" = "__"
      TEST = escape "foo%bar" = "foo_%bar"
      TEST = escape "%foo%" = "_%foo_%"
    end

  let escape_two_orig ~escapeworthy1 ~escapeworthy2 ~escape_char str =
    let len = String.length str in
    let rec loop cnt i =
      if i < 0 then cnt
      else
        let new_i = i - 1 in
        let c = str.[i] in
        if c = escapeworthy1 || c = escapeworthy2 || c = escape_char
        then loop (cnt + 2) new_i
        else loop (cnt + 1) new_i
    in
    let len_1 = len - 1 in
    let cnt = loop 0 len_1 in
    if cnt = len then str
    else
      let res = String.create cnt in
      let rec loop src_pos dst_pos =
        if src_pos < 0 then res
        else
          let new_src_pos = src_pos - 1 in
          let c = str.[src_pos] in
          res.[dst_pos] <- c;
          if c = escapeworthy1 || c = escapeworthy2 || c = escape_char then (
            res.[dst_pos - 1] <- escape_char;
            loop new_src_pos (dst_pos - 2))
          else loop new_src_pos (dst_pos - 1)
      in
      loop len_1 (cnt - 1)

  TEST_MODULE "escape_two_orig" = struct
    let escape = escape_two_orig
      ~escapeworthy1:'%' ~escapeworthy2:'^' ~escape_char:'_'

    TEST = escape "foo" = "foo"
    TEST = escape "_" = "__"
    TEST = escape "foo%bar" = "foo_%bar"
    TEST = escape "^foo%" = "_^foo_%"
  end

  let unescape_gen ~map ~escape_char =
    let get_c_for_code code =
      if code = escape_char then code else
        match List.Assoc.find map code with
        | None -> code
        | Some x -> x
    in
    let count_escape_chars s =
      let ctr = ref 0 in
      let i = ref 0 in
      while !i < String.length s - 1 do
        if s.[!i] = escape_char then
          begin
            incr ctr;
            i := !i + 2
          end
        else
          i := !i + 1
      done;
      !ctr
    in
    let really_unescape_string num_escape_char os =
      let ns_length = String.length os - num_escape_char in
      let ns = String.create ns_length in
      let os_pos = ref 0 in
      for i = 0 to ns_length - 1 do
        if os.[!os_pos] = escape_char then
          begin
            ns.[i] <- get_c_for_code (os.[!os_pos + 1]);
            os_pos := !os_pos + 2;
          end
        else
          begin
            ns.[i] <- os.[!os_pos];
            os_pos := !os_pos + 1;
          end
      done;
      ns
    in
    (fun str ->
       let num_escape_chars = count_escape_chars str in
       if num_escape_chars > 0 then really_unescape_string num_escape_chars str
       else str)

   TEST_MODULE "unescape_gen" = struct
     let unescape = unescape_gen ~map:['p','%';'c','^'] ~escape_char:'_'

     TEST = unescape "foo" = "foo"
     TEST = unescape "__" = "_"
     TEST = unescape "foo_pbar" = "foo%bar"
     TEST = unescape "_cfoo_p" = "^foo%"
  end


  let unescape ~escape_char str = unescape_gen ~map:[] ~escape_char str

  TEST_MODULE "unescape" = struct
    let unescape = unescape ~escape_char:'_'
    TEST = unescape "foo" = "foo"
    TEST = unescape "__" = "_"
    TEST = unescape "foo_%bar" = "foo%bar"
    TEST = unescape "_^foo_%" = "^foo%"
  end

  let rec is_char_escaped ~escape_char str pos =
    if pos >= String.length str || pos < 0
    then invalid_argf "is_char_escaped: out of bounds" ();
    if pos = 0 then false
    else begin
      str.[pos - 1] = escape_char
      && (not (is_char_escaped ~escape_char str (pos - 1)))
    end
  ;;

  TEST_MODULE "is_char_escaped" = struct
    let is = is_char_escaped ~escape_char:'_'
    TEST = is "___" 2 = false
    TEST = is "x" 0 = false
    TEST = is "_x" 1 = true
    TEST = is "sadflkas____sfff" 12 = false
  end

  let is_char_literal ~escape_char str pos =
    if pos >= String.length str || pos < 0
    then invalid_argf "is_literal: out of bounds" ();
    let escaped = is_char_escaped ~escape_char str pos in
    let c = str.[pos] in
    (not escaped && c <> escape_char)
    || (escaped && c = escape_char)
  ;;

  TEST_MODULE "is_char_literal" = struct
    let is_char_literal = is_char_literal ~escape_char:'_'
    TEST = is_char_literal "123456" 4 = true
    TEST = is_char_literal "12345_6" 6 = false
    TEST = is_char_literal "12345_6" 5 = false
    TEST = is_char_literal "123__456" 4 = true
    TEST = is_char_literal "123456__" 7 = true
    TEST = is_char_literal "__123456" 1 = true
    TEST = is_char_literal "__123456" 0 = false
    TEST = is_char_literal "__123456" 2 = true
  end

  let rec index_from ~escape_char str pos char =
    match String.index_from str pos char with
    | None -> None
    | Some pos ->
      if is_char_literal ~escape_char str pos then
        Some pos
      else if pos = String.length str - 1 then
        None
      else
        index_from ~escape_char str (pos + 1) char
  ;;

  let index_from_exn ~escape_char str pos char =
    match index_from ~escape_char str pos char with
    | None -> raise Not_found
    | Some pos -> pos
  ;;

  let index ~escape_char str char = index_from ~escape_char str 0 char
  let index_exn ~escape_char str char = index_from_exn ~escape_char str 0 char

  TEST_MODULE "index_from" = struct
    let f = index_from ~escape_char:'_'
    TEST = f "1273456_7789" 3 '7' = Some 9
  end

  let rec rindex_from ~escape_char str pos char =
    match String.rindex_from str pos char with
    | None -> None
    | Some pos ->
      if is_char_literal ~escape_char str pos then
        Some pos
      else if pos = 0 then
        None
      else
        rindex_from ~escape_char str (pos - 1) char
  ;;

  let rindex_from_exn ~escape_char str pos char =
    match rindex_from ~escape_char str pos char with
    | None -> raise Not_found
    | Some pos -> pos
  ;;

  let rindex ~escape_char str char =
    rindex_from ~escape_char str (String.length str - 1) char
  ;;

  let rindex_exn ~escape_char str char =
    match rindex_from ~escape_char str (String.length str - 1) char with
    | None -> raise Not_found
    | Some pos -> pos
  ;;

  TEST_MODULE "rindex_from" = struct
    let f = rindex_from ~escape_char:'_'
    TEST = f "123456_37839" 9 '3' = Some 2
  end

  let split_gen ~escape_char str ~on =
    let rec char_list_mem l (c:char) =
      match l with
      | [] -> false
      | hd::tl -> hd = c || char_list_mem tl c
    in
    let is_delim on str pos =
      match on with
      | `char c -> str.[pos] = c && is_char_literal ~escape_char str pos
      | `char_list l -> char_list_mem l str.[pos] && is_char_literal ~escape_char str pos
    in
    let len = String.length str in
    let rec loop acc last_pos pos =
      if pos = -1 then
        String.sub str ~pos:0 ~len:last_pos :: acc
      else
        if is_delim on str pos then
          let pos1 = pos + 1 in
          let sub_str = String.sub str ~pos:pos1 ~len:(last_pos - pos1) in
          loop (sub_str :: acc) pos (pos - 1)
        else loop acc last_pos (pos - 1)
    in
    loop [] len (len - 1)
  ;;

  let split str ~on = split_gen str ~on:(`char on) ;;

  let split_on_chars str ~on:chars =
    split_gen str ~on:(`char_list chars)
  ;;

  TEST_MODULE "split_on_gen" = struct
    let split_gen = split_gen ~escape_char:'_' ~on:(`char ',')
    TEST = split_gen "foo,bar,baz" = ["foo"; "bar"; "baz"]
    TEST = split_gen "foo_,bar,baz" = ["foo_,bar"; "baz"]
    TEST = split_gen "foo_,bar_,baz" = ["foo_,bar_,baz"]
    TEST = split_gen "foo__,bar,baz" = ["foo__"; "bar"; "baz"]
    TEST = split_gen "foo,bar,baz_," = ["foo"; "bar"; "baz_,"]
    TEST = split_gen "foo,bar_,baz_,," = ["foo"; "bar_,baz_,"; ""]
  end

  let split2 str ~on ~escape_char f =
    match f ~escape_char str on with
    | None -> None
    | Some pos ->
      Some
        (String.sub str ~pos:0 ~len:pos,
         String.sub str ~pos:(pos + 1) ~len:(String.length str - pos - 1))
  ;;

  let split2_exn str ~on ~escape_char f =
    match split2 str ~on ~escape_char f with
    | None -> raise Not_found
    | Some x -> x
  ;;

  let lsplit2 str ~on ~escape_char = split2 str ~on ~escape_char index
  let rsplit2 str ~on ~escape_char = split2 str ~on ~escape_char rindex
  let lsplit2_exn str ~on ~escape_char = split2_exn str ~on ~escape_char index
  let rsplit2_exn str ~on ~escape_char = split2_exn str ~on ~escape_char rindex

  TEST_MODULE "split2" = struct
    let split2 = split2 ~escape_char:'_' ~on:','
    TEST = split2 "foo_,bar,baz_,0" index = Some ("foo_,bar", "baz_,0")
    TEST = split2 "foo_,bar,baz_,0" rindex = Some ("foo_,bar", "baz_,0")
    TEST = split2 "foo_,bar" index = None
    TEST = split2 "foo_,bar" rindex = None
  end

  (* this implementation is faster, if need be: *)
  (*
  let unescape ~escape_char str =
    let count_escape_chars s =
      let ctr = ref 0 in
      let i = ref 0 in
      while !i < String.length s - 1 do
        if s.[!i] = escape_char then
          begin
            incr ctr;
            i := !i + 2
          end
        else
          i := !i + 1
      done;
      !ctr
    in
    let really_unescape_string num_escape_char os =
      let ns_length = String.length os - num_escape_char in
      let ns = String.create ns_length in
      let os_pos = ref 0 in
      for i = 0 to ns_length - 1 do
        if os.[!os_pos] = escape_char then
          begin
            ns.[i] <- os.[!os_pos + 1];
            os_pos := !os_pos + 2;
          end
        else
          begin
            ns.[i] <- os.[!os_pos];
            os_pos := !os_pos + 1;
          end
      done;
      ns
    in
    let num_escape_chars = count_escape_chars str in
    if num_escape_chars > 0 then really_unescape_string num_escape_chars str
    else str
      *)

end
;;

let consolidate_strings ?(int_sets: [`Exact | `Bounds | `Asterisk] = `Exact) strings =
  (* is_int and tokenize_string could be easier implemented with Pcre, but we do not want
     to introduce the dependency here. *)
  let is_int s =
    let rec is_int i =
      i < 0
      || match String.get s i with
        | '0'..'9' -> is_int (i - 1)
        | _ -> false
    in
    let n = String.length s in
    (n > 0) && is_int (n - 1)
  in
  let tokenize_string s =
    let n = String.length s in
    let rec tokenize_rec i j accum =
      if i >= n then
        List.rev accum
      else
        let char_class = function
          | 'a'..'z' | 'A'..'Z' -> `Alpha
          | '0'..'9' -> `Digit
          | _ -> `Other
        in
        let ci = char_class (String.get s i) in
        let cj = if j < n then char_class (String.get s j) else `Other in
        match ci, cj with
        | `Alpha, `Alpha | `Digit, `Digit -> tokenize_rec i (j + 1) accum
        | _ -> tokenize_rec j (j + 1) ((String.slice s i j)::accum)
    in
    tokenize_rec 0 1 []
  in
  let is_single_letter s =
    String.length s = 1 && ("a" <= s && s <= "z" || "A" <= s && s <= "Z")
  in
  let module P = Pervasives in
  let rec consolidate_token_lists xs =
    let group_subranges ns to_str_fun =
      let rec subranges_str grouped accum =
        match grouped with
        | (h::[])::tt -> subranges_str tt ((to_str_fun h)::accum)
        | (h::t)::tt -> let last = List.last_exn t in
                        let h_last =
                          if h <> last then
                            sprintf "%s-%s" (to_str_fun h) (to_str_fun last)
                          else
                            to_str_fun h
                        in
                        subranges_str tt (h_last::accum)
        | [] -> List.rev accum
        | []::_ -> failwith "Empty group returned by List.group"
      in
      let grouped = List.group ~break:(fun a b -> a + 1 < b) ns in
      String.concat ~sep:"," (subranges_str grouped [])
    in
    let compressed_list ns to_str_fun =
      let ns = List.sort ~cmp:P.compare ns in
      match ns with
      | [] -> ""
      | h::[] -> to_str_fun h
      | _::_::_ -> sprintf "[%s]" (group_subranges ns to_str_fun)
    in
    let rec int_list_opt_of_str_list xs accum is_token_ok_fun to_int_fun =
      match xs with
      | [h]::t ->
        if is_token_ok_fun h then
          int_list_opt_of_str_list t ((to_int_fun h)::accum) is_token_ok_fun to_int_fun
        else
          None
      | [] -> Some (List.rev accum)
      | _ -> None
    in
    let compress_if_all_numbers xs =
      (* [1;3;4;5;8;9;16] -> "1,3-5,8-9,16" *)
      match int_list_opt_of_str_list xs [] is_int int_of_string with
      | None -> None
      | Some ns ->
        match int_sets with
        | `Exact -> Some (compressed_list ns string_of_int)
        | `Bounds ->
          let lo = List.reduce_exn ~f:min ns in
          let hi = List.reduce_exn ~f:max ns in
          let exact = compressed_list ns string_of_int in
          let bounds = sprintf "[%d,??,%d]" lo hi in
          Some (
            if String.length exact < String.length bounds then
              (* two-element sets or contiguous ranges *)
              exact
            else
              bounds)
        | `Asterisk -> Some "*"
    in
    let compress_if_all_single_letters xs =
      (* ["a";"c";"d";"e";"h";"i";"p"] -> "a,c-e,h-i,p" *)
      let fst_char_to_ascii s = Char.to_int (String.get s 0) in
      let ascii_to_str i = String.of_char (Char.of_int_exn i) in
      match int_list_opt_of_str_list xs [] is_single_letter fst_char_to_ascii with
      | None -> None
      | Some ns -> Some (compressed_list ns ascii_to_str)
    in
    let compress_if_all_numbers_or_single_letters xs =
      (* try both of the above *)
      match compress_if_all_numbers xs with
      | Some y -> Some y
      | None -> compress_if_all_single_letters xs
    in
    let first_elts_equal x y =
      match x, y with
      | h1::_, h2::_ when h1 = h2 -> true
      | _ -> false
    in
    let rec group_loop xs accum_groups accum_uniques =
      (* group elts iff they have equal first tokens *)
      match xs, accum_groups, accum_uniques with
      | [], _, _ ->
        (List.rev (List.map ~f:List.rev accum_groups), List.rev accum_uniques)
      | xh::xt, (ghh::ght)::gt, _ when first_elts_equal xh ghh ->
        group_loop xt ((xh::ghh::ght)::gt) accum_uniques
      | xh::xt, _, uh::ut when first_elts_equal xh uh ->
        group_loop xt ((xh::uh::[])::accum_groups) ut
      | xh::xt, _, _ ->
        group_loop xt accum_groups (xh::accum_uniques)
    in
    let group_token_lists xs =
      (* group elts by their first token, and then group the unique ones by their last
         token *)
      let groups, uniques = group_loop xs [] [] in
      (* sort the reversed sequences, so that those with common prefix (originally a
         common suffix) are adjacent  *)
      let rev_uniques = List.sort  ~cmp:P.compare (List.map ~f:List.rev uniques) in
      let groups_last_rev, uniques_last_rev = group_loop rev_uniques [] [] in
      (* now sort each group back after reversing the token sequneces, and sort the
         groups by their first element (that operation is only meant for the output to be
         pretty, it is not essential for the correctness) *)
      let groups_last = List.sort ~cmp:P.compare
        (List.map ~f:(
          fun l -> List.sort ~cmp:P.compare (List.map ~f:List.rev l))
           groups_last_rev) in
      let uniques_last = List.sort ~cmp:P.compare (List.map ~f:List.rev uniques_last_rev)
      in
      (groups @ groups_last, uniques_last)
    in
    let max_common_prefix_suffix xs =
      let rec factor_out_first_elts_if_equal xs accum =
        match xs with
        | (h::t)::[] -> Some (h, List.rev (t::accum))
        | (h1::t1)::(h2::t2)::t when h1 = h2 ->
          factor_out_first_elts_if_equal ((h2::t2)::t) (t1::accum)
        | _ -> None
      in
      let rec common_prefix xs accum_prefix =
        let head_tails = factor_out_first_elts_if_equal xs [] in
        match head_tails with
        | None -> (List.rev accum_prefix, xs)
        | Some (head, tails) -> common_prefix tails (head::accum_prefix)
      in
      let prefix, tails = common_prefix xs [] in
      let rev_tails = List.map ~f:List.rev tails in
      let rev_suffix, rev_infixes = common_prefix rev_tails [] in
      let suffix = List.rev rev_suffix in
      let infixes = List.map ~f:List.rev rev_infixes in
      (prefix, infixes, suffix)
    in
    let consolidate_group (prefix, infixes, suffix) =
      prefix @ (consolidate_token_lists infixes) @ suffix
    in
    match xs with
    | h::[] -> (* nothing to consolidate *)
      h
    | _ ->
      match compress_if_all_numbers_or_single_letters xs with
      | Some s -> [s] (* numbers or single letters only *)
      | None -> (* the general case *)
        let rec consolidate_until_stable xs =
          let groups, uniques = group_token_lists xs in
          let groups_factored_out = List.map ~f:max_common_prefix_suffix groups in
          let groups_consolidated = List.map ~f:consolidate_group groups_factored_out in
          let new_xs = groups_consolidated @ uniques in
          if List.length xs = List.length new_xs then
            new_xs
          else
            consolidate_until_stable new_xs
        in
        let groups_and_uniques = consolidate_until_stable xs in
        match groups_and_uniques with
        | h::[] -> h
        | _ -> (* The group {x,y,z} is now a single token and should not be split apart
                  by further consolidations *)
          [sprintf "{%s}" (
            String.concat ~sep:","
              (List.map ~f:(String.concat ~sep:"") groups_and_uniques))]
  in
  let deduped = List.dedup strings in
  let tokenized = List.map ~f:tokenize_string deduped in
  let sorted = List.sort ~cmp:P.compare tokenized in
  String.concat ~sep:"" (consolidate_token_lists sorted)
;;

TEST_MODULE "consolidate_strings" = struct

  TEST = consolidate_strings
    ["a-b-c-d-e";"a-b-c-d-f";"a-b-c-g-h";"a-b-c-g-i";"a-b-j-k-l"]
  =
    "a-b-{c-{d-[e-f],g-[h-i]},j-k-l}"

  TEST = consolidate_strings
    ["abc-def-1-ghijk";"abc-def-2-ghijk";"abc-def-5-ghijk"; "abc-xyz-2";
     "abc-xyz-3"]
    =
    "abc-{def-[1-2,5]-ghijk,xyz-[2-3]}"

  TEST = consolidate_strings
    ["a-b-10-c-d"; "a-b-14-c-d"; "a-b-12-c-d"; "a-b-13-c-d"; "a-b-11-c-d"]
    =
    "a-b-[10-14]-c-d"

  TEST = consolidate_strings
    ["a-b-10-c-d"; "a-b-14-c-d"; "a-b-12-c-d"; "a-b-13-c-d"; "a-b-c-d"]
    =
    "a-b-{,[10,12-14]-}c-d"

  TEST = consolidate_strings
    ["a-b-10-c-d"; "a-b-14-c-d"; "a-b-12-c-d"; "a-b-13-c-d"; "a-bc-d"]
    =
    "a-{b-[10,12-14]-c,bc}-d"

  TEST = consolidate_strings
    ["a1b1";"x1c1";"y1b1";"z1c1"]
    =
    "{[a,y]1b,[x,z]1c}1"

  TEST = consolidate_strings
    ["a1c";"a1d";"A1c";"A1d";"a2c";"a2d";"A2c";"A2d"]
    =
    "[A,a][1-2][c-d]"

  TEST = consolidate_strings
    ["a1c";"a1d";"A1c";"A1d";"a2c";"a2d";"A2c";"A2d";"D3e"]
    =
    "{D3e,[A,a][1-2][c-d]}"

  TEST = consolidate_strings
    ["a1c";"a1d";"A1c";"A1d";"a2c";"a2d";"A2c";"A2d";"A3e"]
    =
    "{A{3e,[1-2][c-d]},a[1-2][c-d]}"

  TEST = consolidate_strings
    ~int_sets:`Exact
    ["a1c";"a2c";"a4c";"a5c";"a8c";"a10c";"a10c";"a8c";"a11c";"a12c";"a13c"]
    =
    "a[1-2,4-5,8,10-13]c"

  TEST = consolidate_strings
    ~int_sets:`Bounds
    ["a1c";"a2c";"a4c";"a5c";"a8c";"a10c";"a10c";"a8c";"a11c";"a12c";"a13c"]
    =
    "a[1,??,13]c"

  TEST = consolidate_strings
    ~int_sets:`Bounds
    ["a1";"a3";"a5";"a6";"b1";"b2";"b4";"b6"]
    =
    "[a-b][1,??,6]"

  TEST = consolidate_strings
    ~int_sets:`Asterisk
    ["a1c";"a2c";"a4c";"a5c";"a8c";"a10c";"a10c";"a8c";"a11c";"a12c";"a13c"]
    =
    "a*c"

end
;;

let consolidate_strings' ~max_len strings =
  let result = consolidate_strings strings in
  if String.length result <= max_len then
    result
  else
    let result = consolidate_strings ~int_sets:`Bounds strings in
    if String.length result <= max_len then
      result
    else
      let result = consolidate_strings ~int_sets:`Asterisk strings in
      let len = String.length result in
      if len <= max_len then
        result
      else
        let num_dots = min 3 (max 0 (max_len - 1)) in
        let num_chars_from_result = max_len - num_dots in
        String.sub ~pos:0 ~len:num_chars_from_result result ^ String.make num_dots '.'
;;

TEST_MODULE "consolidate_strings" = struct

  TEST = consolidate_strings'
    ~max_len:15
    ["abc1"; "abc2"; "abc4"; "abc8"; "abc16"]
    =
    "abc[1-2,4,8,16]"

  TEST = consolidate_strings'
    ~max_len:14
    ["abc1"; "abc2"; "abc4"; "abc8"; "abc16"]
    =
    "abc[1,??,16]"

  TEST = consolidate_strings'
    ~max_len:12
    ["abc1"; "abc2"; "abc4"; "abc8"; "abc16"]
    =
    "abc[1,??,16]"

  TEST = consolidate_strings'
    ~max_len:11
    ["abc1"; "abc2"; "abc4"; "abc8"; "abc16"]
    =
    "abc*"

  TEST = consolidate_strings'
    ~max_len:4
    ["abc1"; "abc2"; "abc4"; "abc8"; "abc16"]
    =
    "abc*"

  TEST = consolidate_strings'
    ~max_len:5
    ["abcde"]
    =
    "abcde"

  TEST = consolidate_strings'
    ~max_len:5
    ["abcdef"]
    =
    "ab..."

  TEST = consolidate_strings'
    ~max_len:4
    ["abcdef"]
    =
    "a..."

  TEST = consolidate_strings'
    ~max_len:3
    ["abc1"; "abc2"; "abc4"; "abc8"; "abc16"]
    =
    "a.."

  TEST = consolidate_strings'
    ~max_len:2
    ["abc1"; "abc2"; "abc4"; "abc8"; "abc16"]
    =
    "a."

  TEST = consolidate_strings'
    ~max_len:1
    ["abc1"; "abc2"; "abc4"; "abc8"; "abc16"]
    =
    "a"

  TEST = consolidate_strings'
    ~max_len:0
    ["abc1"; "abc2"; "abc4"; "abc8"; "abc16"]
    =
    ""

end
;;
