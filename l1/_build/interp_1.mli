
type address = int 

type value = 
     | INT of int 
     | BOOL of bool
     | SKIP 

and closure = Ast.var * Ast.expr * env 

and continuation_action = 
  | OPER of Ast.oper * value
  | OPER_FST of Ast.expr * env * Ast.oper 
  | TAIL of Ast.expr list * env
  | ASN of int * env
  | IF of Ast.expr * env * Ast.expr
  | WHILE of Ast.expr * env * Ast.expr

and continuation = continuation_action  list

and binding = Ast.var * value

and env = binding list

type state = 
   | EXAMINE of Ast.expr * env * continuation 
   | COMPUTE of continuation * value 

val step : state -> state 

val driver : int -> state -> value 

val eval : Ast.expr * env -> value 

val interpret : Ast.expr -> value 

val string_of_value : value -> string 

