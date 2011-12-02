%{
  (** Parser: Grammar Specification for Parsing S-expressions *)

  open Lexing

  let parse_failure what =
    let pos = symbol_start_pos () in
    let msg =
      Printf.sprintf "Sexplib.Parser: failed to parse line %d char %d: %s"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol) what in
    failwith msg
%}

%token <string> STRING
%token LPAREN RPAREN EOF

%start sexp
%type <Type.t> sexp

%start sexp_opt
%type <Type.t option> sexp_opt

%start sexps
%type <Type.t list> sexps

%start rev_sexps
%type <Type.t list> rev_sexps

%%

sexp
  : STRING { Type.Atom $1 }
  | LPAREN RPAREN { Type.List [] }
  | LPAREN rev_sexps_aux RPAREN { Type.List (List.rev $2) }
  | error { parse_failure "sexp" }

sexp_opt
  : sexp { Some $1 }
  | EOF { None }

rev_sexps_aux
  : sexp { [$1] }
  | rev_sexps_aux sexp { $2 :: $1 }

rev_sexps
  : rev_sexps_aux { $1 }
  | EOF { [] }

sexps
  : rev_sexps_aux { List.rev $1 }
  | EOF { [] }
