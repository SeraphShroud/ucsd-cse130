(* CSE 130: Programming Assignment 2
 * misc.ml
 * Author: Evan Carey (U06430877)
 *)

(* assoc : int * string * (string * int) list -> int
 * assoc(d,k,l)  searches a list l of key-value pairs [(k1,v1);(k2,v2);...] 
 * and finds the first element (k_i,v_i) s.t. k_i = k, returning v_i.
 * If no such element is found, the default value d is returned.
 *)

let rec assoc (d,k,l) = 
  match l with
  | [] -> d
  | (k1,v1)::t -> 
    if k1 = k then v1 
    else assoc (d, k, t)
;;

(* removeDuplicates : 'a list -> 'a list
 * (removeDuplicates l) reads a list l and constructs a new list with all
 * duplicated elements of l removed, preserving order.
 * E.g. (removeDuplicates [1;3;2;1;4;4]) is [1;3;2;4]
 *      (removeDuplicates ["a";"c";"c";"b";"a"]) is ["a";"c";"b"]
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = if (List.mem h seen) then seen else (h :: seen) in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))
;;


(* wwhile : ('a -> 'a * bool) * 'a -> 'a
 * A recursive implementation of a while loop.
 * wwhile (f,b) calls the function f on input b to get a pair (b',c'). f will
 * repeatedly be called on b' until c' is false, then wwhile will return b'
 * E.g. Given (let f x = let xx = x*x*x in (xx,xx<100)), wwhile (f,2) is 512
 *)
let rec wwhile (f,b) = 
  let (b',c') = f b in
  if c' then wwhile (f,b')
  else b'
;;


(* fixpoint : ('a -> 'a) * 'a -> 'a
 * fixpoint(f,b) repeatedly updates b with f(b) until b = f(b) and then returns b.
 * E.g. Given (let g x = truncate (1e6 *. cos(1e-6 *. float x))), 
 *       fixpoint (g,0) is 739085 (because, in radians, cos(.739085) = .739085)
 *)
let fixpoint (f,b) = wwhile (
  let aux b =
    let c = f b in 
      if c = b then (c, false)
      else (c, true)
  in aux
,b);;


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
