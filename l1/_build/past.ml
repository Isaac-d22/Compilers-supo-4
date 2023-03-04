(* 

   The Parsed AST 

*) 
type var = string 

type loc = Lexing.position 

type type_expr = 
   | TEint
   | TEbool 
   | TEunit 

type formals = (var * type_expr) list

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

and lambda = var * type_expr * expr 

let  loc_of_expr = function 
    | Integer (loc, _)              -> loc
    | Bool (loc, _)                 -> loc
    | Op(loc, _, _, _)              -> loc 
    | Assign(loc, _, _)             -> loc
    | Deref(loc, _)                 -> loc
    | If(loc, _, _, _)              -> loc
    | While(loc, _, _)              -> loc
	| Seq(loc, _)                   -> loc


let string_of_loc loc = 
    "line " ^ (string_of_int (loc.Lexing.pos_lnum)) ^ ", " ^ 
    "position " ^ (string_of_int ((loc.Lexing.pos_cnum - loc.Lexing.pos_bol) + 1))

open Format

(*
   Documentation of Format can be found here: 
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*) 

let rec pp_type = function 
  | TEint -> "int" 
  | TEbool -> "bool" 
  | TEunit -> "unit" 

let pp_bop = function 
  | ADD -> "+" 
  | GTE -> ">="

let string_of_oper = pp_bop 

let fstring ppf s = fprintf ppf "%s" s
let pp_type ppf t = fstring ppf (pp_type t) 
let pp_binary ppf op = fstring ppf (pp_bop op) 

(* ignore locations *) 
let rec pp_expr ppf = function 
    | Integer (_, n)      -> fstring ppf (string_of_int n)
    | Bool (_, b)         -> fstring ppf (string_of_bool b)
    | Op(_, e1, op, e2)   -> fprintf ppf "(%a %a %a)" pp_expr e1  pp_binary op pp_expr e2 
    | Assign(_, l, e)     -> fprintf ppf "%d:=%a" l pp_expr e
    | Deref (_, l)        -> fprintf ppf "!%d" l
    | If (_, e1, e2, e3)  -> fprintf ppf "if %a then do %a else %a" pp_expr e1 pp_expr e2 pp_expr e3
    | While (_, e1, e2)   -> fprintf ppf "while %a do %a" pp_expr e1 pp_expr e2
    | Seq (_, [])         -> () 
    | Seq (_, [e])        -> pp_expr ppf e 
    | Seq (l, e :: rest)  -> fprintf ppf "%a; %a" pp_expr e pp_expr (Seq(l, rest))	

let print_expr e = 
    let _ = pp_expr std_formatter e
    in print_flush () 

let eprint_expr e = 
    let _ = pp_expr err_formatter e
    in print_flush () 

(* useful for degugging *) 

let string_of_bop = function 
  | ADD -> "ADD" 
  | GTE  -> "GTE" 

let mk_con con l = 
    let rec aux carry = function 
      | [] -> carry ^ ")"
      | [s] -> carry ^ s ^ ")"
      | s::rest -> aux (carry ^ s ^ ", ") rest 
    in aux (con ^ "(") l 

let rec string_of_type = function 
  | TEint             -> "TEint" 
  | TEbool            -> "TEbool" 
  | TEunit            -> "TEunit" 

let rec string_of_expr = function 
    | Integer (_, n)      -> mk_con "Integer" [string_of_int n] 
    | Bool (_, b)         -> mk_con "Bool" [string_of_bool b] 
    | Assign(_, l, e) -> mk_con "Assign" [string_of_int l; string_of_expr e]
    | Deref (_, l)   -> mk_con "Deref" [string_of_int l]
    | If (_, e1, e2, e3)  -> mk_con "If" [string_of_expr e1; string_of_expr e2; string_of_expr e3]
    | While (_, e1, e2)   -> mk_con "While" [string_of_expr e1; string_of_expr e2]
    | Op(_, e1, op, e2)   -> mk_con "Op" [string_of_expr e1; string_of_bop op; string_of_expr e2]
    | Seq (_, el)         -> mk_con "Seq" [string_of_expr_list el]


and string_of_expr_list = function 
  | [] -> "" 
  | [e] -> string_of_expr e 
  |  e:: rest -> (string_of_expr e ) ^ "; " ^ (string_of_expr_list rest)
