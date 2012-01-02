open Camlp4.PreCast
let f =
  object (self)
     inherit Ast.map as super
     method expr = function
       | Ast.ExApp(_loc,Ast.ExApp(_,Ast.ExId(_,Ast.IdLid(_,"|!")),x),y)  ->
         <:expr<$y$ $self#expr x$>>
       | Ast.ExId(_loc,Ast.IdLid(_,"|!")) ->
         Loc.raise _loc (Failure "|! must be applied to two arguments")
       | e -> super#expr e
  end in
AstFilters.register_str_item_filter f#str_item
