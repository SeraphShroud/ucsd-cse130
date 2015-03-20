(* CSE 130: Programming Assignment 3
 * misc.ml
 * Author: Evan Carey (U06430877)
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(* sqsum : int list -> int
 * Takes a list of integers [x1;...;xn] and returns the integer x1^2 + ... + xn^2
 *)
let sqsum xs = 
  let f a x = a + (x*x) in
  let base = 0 in
    List.fold_left f base xs
;;

(* pipe : ('a -> 'a) list -> ('a -> 'a) 
 * Takes a list of functions [f1;...;fn] and return a function f 
 * such that for any x, the application f x returns the result fn(...(f2(f1 x)))
 *)
let pipe fs = 
  let f a x = fun h -> x (a h) in
  let base = fun x -> x in
    List.fold_left f base fs
;;

(* sepConcat : string -> string list -> string
 * Concatenates the strings in list (sl) into a single string, using (sep) as
 * a separater
 *)
let rec sepConcat sep sl = 
  match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x in
      let base = h in
      let l = t in
        List.fold_left f base l
;;

(* stringOfList : ('a -> string) -> 'a list -> string
 * Returns a string representation of the list l as a concatenation of 
 * "[" (fl1) ";" (fl2) ";" ... ";" (fln) "]"
 *)
let stringOfList f l = "[" ^ (sepConcat "; " (List.map f l)) ^ "]"
;;


(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)


(* clone : 'a -> int -> 'a list
 * (clone x n) creates a list of length (n), where each element is (x).
 * If n is 0 or negative, it returns the empty list.
 *)
let rec clone x n = 
  if n <= 0 then []
  else x::(clone x (n-1))
;;

(* padZero : int list -> int list -> int list * int list
 * Takes two list [x1;...;xn] [y1;...;ym] and adds zeros in front to 
 * make them equal length, returning them as a pair
 * E.g. (padZero [9;9] [1;0;0;2]) => ([0;0;9;9], [1;0;0;2])
 *)
let rec padZero l1 l2 = 
  let diff = List.length l1 - List.length l2 in
  let zs = clone 0 (abs diff) in
  if diff <= 0 then (zs @ l1, l2)
  else (l1, zs @ l2)
;;

(* removeZero : int list -> int list
 * Takes a list and removes a prefix of trailing zeros.
 *)
let rec removeZero l = 
  match l with
  | [] -> l
  | h::t -> if h = 0 then removeZero t else l
;;

(* bigAdd : int list -> int list -> int list
 * Takes two integer lists, where each element is in the range [0..9] and
 * returns the list corresponding to the sum of the two big integers.
 * E.g. (bigAdd [9;9] [1;0;0;2]) evaluates to [1;1;0;1]
 *      (bigAdd [9;9;9;9] [9;9;9]) evaluates to [1;0;9;9;8]
 *)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = 
      let (x1,x2) = x in
      let (carry, result_so_far) = a in
      let sum = x1 + x2 + carry in
      let new_carry = sum / 10 in
      let remainder = sum mod 10 in
      let new_acc = remainder::result_so_far in
      
      if (List.length new_acc = List.length l1) (* if true, we've finished processing input *)
      then (0, new_carry::new_acc) (* so we must append carry to head of sum or it will be thrown out *)
      else (new_carry, new_acc)
    in

    let base = (0,[]) in
    let args = List.rev (List.combine l1 l2) in (*args -> (int * int) list *)
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))
;;

(* mulByDigit : int -> int list -> int list
 * (mulByDigit i l) takes a big integer (represented as a list) l
 * and multiplies it by an integer digit i (0 <= i < 10), returning the result as a list.
 * E.g. (mulByDigit 9 [9;9;9;9]) evaluates to [8;9;9;9;1]
 *)
let rec mulByDigit i l = 
  (* sanity check that i is a digit *)
  if (i < 0 || i > 9) then []
  else
    let multiply i l =
      let base = (0,[]) in
      let f a x =
        let (carry, acc) = a in
        let product = (x * i) + carry in
        let new_carry = product / 10 in
        let remainder = product mod 10 in
        let new_acc = remainder::acc in
        if (List.length new_acc = List.length l) then (0,new_carry::new_acc)
        else (new_carry,new_acc)
      in
      let (_,res) = List.fold_left f base (List.rev l) in
        res
    in
      removeZero (multiply i l)
;;


(* bigMul : int list -> int list -> int list 
 * Takes two integer lists, where each element is in the range [0..9] and
 * returns the list corresponding to the product of the two big integers.
 * E.g. (bigMul [9;9;9;9] [9;9;9;9]) evaluates to [9;9;9;8;0;0;0;1]
 *      (bigMul [2] [1;0]) evaluates to [2;0]
 *)
let bigMul l1 l2 = 
  let f a x = 
    let (x1, x2) = x in (* x : (int list * int) *)
    let product = mulByDigit x2 x1 in
    let (padding, sum_so_far) = a in (* a : (int * int list) *)
    let pad_list = clone 0 padding in (* convert padding to list of 0s (e.g. 2 -> [0;0]) *)
    let new_sum = 
      (* pad product with 0s and add it to the current sum *)
      bigAdd sum_so_far (product @ pad_list)
    in 
    (padding + 1, new_sum)
  in
  let base = (0,[]) in
  let args = (* args : (int list * int) list *)
    (* a list of tuples. Each tuple is ( (entirety of l1 : int list), (digit of l2 : int) ) *)
    let (l1', l2') = padZero l1 l2 in
    List.rev (List.combine (clone l1' (List.length l2')) l2') 
  in
  let (_, res) = List.fold_left f base args in
    res
;;
