(* CSE 130: Programming Assignment 4
 * nano.ml
 * Author: Evan Carey (U06430877) 
 *)

exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | NilExpr -> "[]"

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(* lookup : string * (env : (string * value) list) -> value
 * Finds the most recent binding for a variable (i.e. the first from the left)
 * in the list represent the environment.
 *)
let lookup (x,evn) = 
  match listAssoc(x,evn) with
  | Some x -> x
  | _ -> raise (MLFailure ("variable not bound: " ^ x))
;;

(* eval : (env : (string * value) list) * expr -> value
 * (eval (evn,e)) evaluates an ML-nano expression e, in the environment evn,
 * and raises an exception MLFailure if the expression contains an unbound variable.
 *)
let rec eval (evn,e) = 
  match e with
  | Const i -> Int i
  | True -> Bool true
  | False -> Bool false
  | NilExpr -> Nil
  | Var s -> lookup (s,evn)
  | Bin (e1,op,e2) -> (
    let x = eval (evn, e1) in
    let y = eval (evn, e2) in
    match (x,op,y) with
    | Int x, Plus, Int y -> Int (x + y)
    | Int x, Minus, Int y -> Int (x - y)
    | Int x, Mul, Int y -> Int (x * y)
    | Int x, Div, Int y -> Int (x / y)
    | Int x, Eq, Int y -> Bool (x = y)
    | Int x, Ne, Int y -> Bool (x != y)
    | Int x, Lt, Int y -> Bool (x < y)
    | Int x, Le, Int y -> Bool (x <= y)
    | Bool x, Eq, Bool y -> Bool (x = y)
    | Bool x, Ne, Bool y -> Bool (x != y)
    | Bool x, And, Bool y -> Bool (x && y)
    | Bool x, Or, Bool y -> Bool (x || y)
    | Int x, Cons, Nil -> Pair (Int x, Nil)
    | Int x, Cons, Pair(a,b) -> Pair (Int x, y)
    | _ -> raise (MLFailure ("Unsupported operation: " ^ exprToString e))
  )
  | If (p,t,f) -> (
    match eval (evn, p) with
    | Bool b -> if b then eval (evn,t) else eval (evn,f)
    | _ -> raise (MLFailure ("Expression (" ^ exprToString p ^ ") does not evaluate to a Bool"))
  )
  | Let (b,e1,e2) -> (
    let evn = (b, eval (evn,e1))::evn in eval (evn,e2)
  )
  | Letrec (b,e1,e2) -> (
    let v = eval (evn, e1) in
    let evn1 = (
      match v with
      | Closure (evn',None,x,e) -> Closure (evn',Some b,x,e)
      | _ -> v
    ) in
    let evn2 = (b,evn1)::evn in eval (evn2,e2)
  )
  | App (e1,e2) -> (
    let Closure (evn1,f,x,e) = eval (evn,e1) in
    let v = eval (evn, e2) in
    let evn' = (
      match f with
      | Some n -> (n, Closure (evn1,f,x,e))::(x,v)::evn1
      | None -> (x,v)::evn1
    ) in eval (evn', e)
  )
  | Fun (x,e) -> Closure (evn, None, x, e)
