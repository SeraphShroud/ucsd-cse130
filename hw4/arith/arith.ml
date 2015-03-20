(*
  Arithmetic expressions with constant integers, variables, and the four
  standard binary operators, such as:

  3 - 4 * (x + 5)

  can be represented with a recursive binary tree data structure, whose leaf
  nodes are constants or variables, and whose inner nodes are the operators:

    -
   / \
  3   *
     / \
    4   +
       / \
      x   5
*)

type arith_expr =
  | Const of int
  | Var of string
  | Plus of arith_expr * arith_expr
  | Minus of arith_expr * arith_expr
  | Times of arith_expr * arith_expr
  | Divide of arith_expr * arith_expr

(*
  So, the example above would be:

  Minus (Const 3,
         Times (Const 4,
                Plus (Var "x",
                      Const 5)))

  Because arith_exprs are recursive in nature, we'll write a recursive
  function to evaluate them. The evaluation function takes as input an
  environment, a (string * int) list that maps variables to values.
*)

let rec eval_expr env = function
  | Const i -> i
  | Var s -> List.assoc s env
  | Plus (e1, e2) -> eval_expr env e1 + eval_expr env e2
  | Minus (e1, e2) -> eval_expr env e1 - eval_expr env e2
  | Times (e1, e2) -> eval_expr env e1 * eval_expr env e2
  | Divide (e1, e2) -> eval_expr env e1 / eval_expr env e2
