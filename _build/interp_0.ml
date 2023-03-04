(**************************************
Compiler Construction 2021 - Supo
mjp217@cam.ac.uk

Very much using the structure of the Slang interpreter provided by Tim Griffin
*****************************************) 

(*  Interpreter 0 for L1 

    This is a "definitional" interpreter for  for language L1 
    using high-level constructs of Ocaml (the defining language). 

	The interpreter is deliberately incomplete - we will use the supervisions to populate...
*) 
open Ast 

let complain = Errors.complain

let verbose = ref false 

type address = int 

type store = address -> value

and value = 
     | INT of int 
     | BOOL of bool
     | SKIP

type env = var -> value 

type binding = var * value

type bindings = binding list

(* auxiliary functions *) 

let rec string_of_value = function 
     | INT n -> string_of_int n
     | BOOL b -> string_of_bool b
     | SKIP -> "skip" 
    
(* update : (env * binding) -> env 
   update : (store * (address * value)) -> store
*) 
let update(env, (x, v)) = fun y -> if x = y then v else env y

let readint () = let _ = print_string "input> " in read_int() 

let do_oper = function 
  | (ADD,  INT m,   INT n)  -> INT (m + n)
  | (GTE,  INT m,   INT n)  -> BOOL(m >= n)
  | (op, _, _)  -> complain ("malformed binary operator: " ^ (string_of_oper op))

(*
    interpret : (expr * env * store) -> (value * store) 
              : (expr * (var -> value) * address -> value) -> value
*) 
let rec interpret (e, env, store) = 
    match e with 
	| Integer n        -> (INT n, store) 
  | Skip             -> (SKIP, store)
  | Bool b           -> (BOOL b, store)
    | Op(e1, op, e2)   -> let (v1, store1) = interpret(e1, env, store) in 
                          let (v2, store2) = interpret(e2, env, store1) in (do_oper(op, v1, v2), store2) 
    | Seq [e]          -> interpret (e, env, store)
    | Seq (e :: rest)  -> let (_,  store1) = interpret(e, env, store) 
                          in interpret(Seq rest, env, store1)
    | Assign(l, e1)    -> let (v1, store1) = interpret(e1, env, store) in
                          let store2 = update(store1, (l, v1)) in (SKIP, store2)
    | Deref l -> (store l, store)
    | If(e1, e2, e3)   -> let (BOOL(b), store1) = interpret(e1, env, store) in 
                          if b then interpret(e2, env, store1)
                          else interpret(e3, env, store1)
    | While(e1, e2)    -> let (BOOL(b), store1) = interpret(e1, env, store) in 
                          interpret(Seq([e2 ;If(e1, While(e1, e2), Skip)]), env, store1)

(* env_empty : env *) 
let empty_env = fun x -> complain (x ^ " is not defined!\n")

(* store_empty : env *) 
let empty_store = fun x -> complain ((string_of_int x) ^ " is not allocated!\n")

(* interpret_top_level : expr -> value *) 
let interpret_top_level e = let (v, _) = interpret(e, empty_env, empty_store) in v 
    


      
    
    
