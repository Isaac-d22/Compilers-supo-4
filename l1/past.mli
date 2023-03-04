(* 
   The Parsed AST 
*) 
type var = string 

type loc = Lexing.position 

type type_expr = 
   | TEint 
   | TEbool 
   | TEunit 

type oper = ADD | GTE



type expr = 
       | Integer of loc * int
       | Bool of loc * bool
       | Deref of loc * int
       | Assign of loc * int * expr
       | If of loc * expr * expr * expr
       | While of loc * expr * expr
       | Op of loc * expr * oper * expr
       | Seq of loc * (expr list)

val loc_of_expr : expr -> loc 
val string_of_loc : loc -> string 

(* printing *) 
val string_of_oper : oper -> string 
val string_of_type : type_expr -> string 
val string_of_expr : expr -> string 
val print_expr : expr -> unit 
val eprint_expr : expr -> unit


