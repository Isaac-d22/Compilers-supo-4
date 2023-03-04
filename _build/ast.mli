
type var = string 

type oper = ADD | GTE | ASSIGN


type expr = 
       | Integer of int
       | Bool of bool
       | Skip
       | Assign of int * expr
       | Deref of int
       | If of expr * expr * expr
       | While of expr * expr
       | Op of expr * oper * expr
       | Seq of (expr list)

(* printing *) 
val string_of_oper : oper -> string 
val string_of_bop : oper -> string 
val print_expr : expr -> unit 
val eprint_expr : expr -> unit
val string_of_expr : expr -> string 
