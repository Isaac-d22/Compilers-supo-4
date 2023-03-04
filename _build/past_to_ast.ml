(*   translate_expr : Past.expr -> Ast.expr 
     
	 Lifted and amended from the original Slang interpreter

*) 

let translate_bop = function 
  | Past.ADD -> Ast.ADD 
  | Past.GTE -> Ast.GTE

let rec translate_expr = function 
    | Past.Integer(_, n)     -> Ast.Integer n
    | Past.Bool(_, b)        -> Ast.Bool b
    | Past.Deref(_, l)       -> Ast.Deref (l)
    | Past.Assign(_, l, e)   -> Ast.Assign(l, translate_expr e)
    | Past.If(_, e1, e2, e3) -> Ast.If(translate_expr e1, translate_expr e2, translate_expr e3)
    | Past.While(_, e1, e2)  -> Ast.While(translate_expr e1, translate_expr e2)
    | Past.Op(_, e1, op, e2) -> Ast.Op(translate_expr e1, translate_bop op, translate_expr e2)
    | Past.Seq(_, e1) -> Ast.Seq(List.map translate_expr e1)

