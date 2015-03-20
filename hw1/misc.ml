(* CSE 130: Programming Assignment 1
 * misc.ml
 * Author: Evan Carey (U06430877)
 *)

(* sumList : int list -> int 
 * Recursively compute the sum of the elements in an int list.
 * e.g. (sumList [1;2;3]) is 6
 *      (sumList [0;-10;1]) is -9
 *) 

let rec sumList l =
    match l with
    | [] -> 0
    | h::t -> h + sumList t
;;


(* append: 'a list -> 'a list -> 'a list
 * Appends two lists of the same type. Analogous to the (@) operator.
 * e.g. (append [1;2;3] [4;6;5]) is [1;2;3;4;6;5]
 *      (append [] ["a"]) is ["a"]
 *)

let rec append l1 l2 =
    match l1 with
    [] -> l2
    | h::t -> h :: (append t l2)
;;


(* digitsOfInt : int -> int list 
 * Given a positive integer n, (digitsOfInt n) constructs a list of 
 * the digits of n in the order they appear in n.
 * e.g. (digitsOfInt 9876) is [9;8;7;6]
 *)

let rec digitsOfInt n = 
    if n  = 0 then []
    else append (digitsOfInt (n/10)) [n mod 10]
;;


(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422)) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* additivePersistence : int -> int
 * Given a positive integer n, (additivePersistence n) is the number of
 * additions needed to compute the digital root of n. 
 *)

let rec additivePersistence n = 
    if n < 10 then 0
    else 1 + additivePersistence(sumList(digitsOfInt(n)))
;;


(* digitalRoot : int -> int
 * Given a positivie integer n, (digitalRoot n) is the single-digit result 
 * of recursively summing the digits in n.
 *)

let rec digitalRoot n = 
    if n < 10 then n
    else digitalRoot(sumList(digitsOfInt(n)))
;;


(* removeLast : 'a list -> 'a list
 * Given a non-empty list l, (removeLast l) will remove the last element of l.
 * If l is empty, (removeLast l) will raise an exception.
 * e.g. (removeLast [1;2;3]) is [1;2]
 *      (removeLast ["a"]) is []
 *)

let rec removeLast l = 
    match l with
    | [] -> raise (Failure "Empty list")
    | h::(b::_ as t) -> h::removeLast t
    | [h] -> []
;;

(* last : 'a list -> 'a
 * Given a non-empty list l, (last l) will evaluate to the last element of l.
 * If l is empty, (last l) will raise an exception.
 * e.g. (last [1;2;3]) is 3
 *      (last ["a"]) is "a"
 *)

let rec last l =
    match l with
    | [] -> raise (Failure "Empty list")
    | [h] -> h
    | h::t -> last t
;;

(* listReverse : 'a list -> 'a list
 * Given a list l, (list Reverse l) will reverse the order of the elements in l
 * e.g. (listReverse [1;2;3]) is [3;2;1]
 *      (listReverse ["a";"b"]) is ["b";"a"]
 *)

let rec listReverse l = 
    match l with
    | [] -> []
    | h::(b::_ as t) -> last t :: listReverse(removeLast l)
    | [h] -> [h]
;;


(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)

let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0


(* palindrome : string -> bool 
 * Given a string w, (palindrome w) is whether w is a palindrome.
 * NOTE: it is case-sensitve and whitespace is not ignored.
 * e.g. (palindrome "racecar") is true
 *      (palindrome "Racecar") is false
 *)

let palindrome w = 
    explode w = listReverse (explode w)
;;

