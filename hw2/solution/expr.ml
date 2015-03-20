(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* REMEMBER TO DOCUMENT ALL FUNCTIONS THAT YOU WRITE OR COMPLETE *)

type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr
  (* added operators *)
  | Negative of expr
  | Average3 of expr * expr * expr

(* exprToString : expr -> string
 * Enables the printing of expressions
 *)
let rec exprToString e = 
  match e with
  | VarX -> "x"
  | VarY -> "y"
  | Sine e'-> "sin(pi*" ^ exprToString e' ^ ")"
  | Cosine e'-> "cos(pi*" ^ exprToString e' ^ ")"
  | Average (a,b) -> "((" ^ exprToString a ^ "+" ^ exprToString b ^ ")/2)"
  | Times (a,b) -> exprToString a ^ "*" ^ exprToString b
  | Thresh (a,b,c,d) -> "(" ^ exprToString a ^ "<" ^ exprToString b ^ "?" ^ exprToString c ^ ":" ^ exprToString d ^ ")"
  (* added operators *)
  | Negative e' -> "((-1.0)*"^ exprToString e' ^ ")"
  | Average3 (a,b,c) -> "((" ^ exprToString a ^ "+" ^ exprToString b ^ "+" ^ exprToString c ^ ")/3)"
;;

(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
(* added build functions *)
let buildNegative(e)               = Negative(e)
let buildAverage3(e1,e2,e3)        = Average3(e1,e2,e3)


let pi = 4.0 *. atan 1.0

(* eval : expr * float * float -> float
 * (eval (e,x,y)) evaluates the expression e at the point x,y
 *)
let rec eval (e,x,y) = 
  match e with
  | VarX -> x
  | VarY -> y
  | Sine (e') -> sin(pi *. (eval (e',x,y)) )
  | Cosine (e') -> cos(pi *. (eval (e',x,y)) )
  | Average (a,b) -> (eval (a,x,y) +. eval (b,x,y)) /. 2.0
  | Times (a,b) -> eval (a,x,y) *. eval (b,x,y)
  | Thresh (a,b,c,d) -> if ((eval (a,x,y) ) < (eval (b,x,y))) then eval (c,x,y) else eval (d,x,y)
  (* added operators *)
  | Negative (e') -> (-1.0) *. eval (e',x,y)
  | Average3 (a,b,c) -> (eval (a,x,y) +. eval (b,x,y) +. eval (c,x,y)) /. 3.0
;;

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)
