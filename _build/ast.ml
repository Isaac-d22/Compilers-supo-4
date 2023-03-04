
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

and lambda = var * expr 


open Format

(*
   Documentation of Format can be found here: 
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*) 


let pp_bop = function 
  | ADD -> "+" 
  | GTE -> ">="
  | ASSIGN -> ":="

let string_of_oper = pp_bop 

let fstring ppf s = fprintf ppf "%s" s

let pp_binary ppf t = fstring ppf (pp_bop t) 

let rec pp_expr ppf = function 
    | Integer n        -> fstring ppf (string_of_int n)
    | Bool b           -> fstring ppf (string_of_bool b)
    | Assign(l, e)     -> fprintf ppf "%d:=%a" l pp_expr e
    | Deref l          -> fprintf ppf "!%d" l
    | If(e1, e2, e3)   -> fprintf ppf "if %a then %a else %a" pp_expr e1 pp_expr e2 pp_expr e3
    | Op(e1, op, e2)   -> fprintf ppf "(%a %a %a)" pp_expr e1  pp_binary op pp_expr e2 

    | Seq el           -> fprintf ppf "begin %a end" pp_expr_list el 
	
and pp_expr_list ppf = function 
  | [] -> () 
  | [e] -> pp_expr ppf e 
  |  e:: rest -> fprintf ppf "%a; %a" pp_expr e pp_expr_list rest 


let print_expr e = 
    let _ = pp_expr std_formatter e
    in print_flush () 

let eprint_expr e = 
    let _ = pp_expr err_formatter e
    in pp_print_flush err_formatter () 

(* useful for debugging *) 

let string_of_bop = function 
  | ADD -> "ADD" 
  | GTE -> "GTE"

let mk_con con l = 
    let rec aux carry = function 
      | [] -> carry ^ ")"
      | [s] -> carry ^ s ^ ")"
      | s::rest -> aux (carry ^ s ^ ", ") rest 
    in aux (con ^ "(") l 

let rec string_of_expr = function 
    | Integer n        -> mk_con "Integer" [string_of_int n] 
    | Bool b           -> mk_con "Bool" [string_of_bool b] 
    | Assign(l, e) -> mk_con "Assign" [string_of_int l; string_of_expr e]
    | Deref l   -> mk_con "Deref" [string_of_int l]
    | If(e1, e2, e3)   -> mk_con "If" [string_of_expr e1; string_of_expr e2; string_of_expr e3]
    | While(e1, e2)    -> mk_con "While" [string_of_expr e1; string_of_expr e2]
    | Op(e1, op, e2)   -> mk_con "Op" [string_of_expr e1; string_of_bop op; string_of_expr e2]
    | Seq el           -> mk_con "Seq" [string_of_expr_list el] 

and string_of_expr_list = function 
  | [] -> "" 
  | [e] -> string_of_expr e 
  |  e:: rest -> (string_of_expr e ) ^ "; " ^ (string_of_expr_list rest)

