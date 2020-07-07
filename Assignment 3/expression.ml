(*** CS5035 Assignment 3 Part 2: A Language for Symbolic Differentiation [75 POINTS] ***)

(* Make sure that your submission does NOT have
 * both compiling errors and running errors. *)

(* You are free to use any function from the Pervasives module (http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html).
 *
 * You should NOT use functions from the Float module (https://caml.inria.fr/pub/docs/manual-ocaml/libref/Float.html).
*)

(* You are free to define auxiliary functions.
 * Just make sure that if we name a particular function that
 * you have to write (either in the assignment text, or in a 
 * template file), that you preserve its name so that our 
 * automated grader can find it.
*)

(* TIPS FOR PART 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry 
 *    about expressionLibrary.ml
 * 3. Test!  (Use "assert" where appropriate.)
*)

open Ast;;
open ExpressionLibrary;;

let undefined (s:string) : 'a = failwith (s^" undefined");;
let unimplemented (s:string) : 'a = failwith (s^" unimplemented");;
(* ====== Don't remove the above lines. ====== *)




(*>* Problem 2.1 [5 POINTS] *>*)
(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false 
 *
 * parse is a function with type string -> expression (defined in expressionLibrary.ml)
*)
let rec contains_var (e:expression) : bool =
  match e with
  | Num z -> false 
  | Var -> true
  | Binop (_, x, y) -> (contains_var x) || (contains_var y) 
  | Unop (_, x) -> contains_var x
;;




(*>* Problem 2.2 [10 POINTS] *>*)
(* evaluate : evaluates an expression for a particular value of x. Use OCaml's
 *            built-in function (i.e. function (/.) ) of handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
(* Hint: Use OCaml's built-in functions sin, cos, log, (~-.) to
 *       evaluate unary operators Sin, Cos, Ln, Neg, respectively.
 * See the Pervasives module for details:*
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
*)
let rec evaluate (e:expression) (x:float) : float =
  let rec power (base: float) (exp: int) : float = 
    match exp with
    | 0 -> 1.
    | n -> base *. (power base (n-1))
  in
  match e with 
  | Num z -> z
  | Var -> x
  | Binop (op, a, b) -> (
    match op with 
    | Add -> (evaluate a x) +. (evaluate b x)
    | Sub -> (evaluate a x) -. (evaluate b x)
    | Mul -> (evaluate a x) *. (evaluate b x)
    | Div -> (evaluate a x) /. (evaluate b x)
    | Pow -> power (evaluate a x) (int_of_float (evaluate b x)))
  | Unop (op, a) -> (
    match op with 
    | Sin -> sin (evaluate a x)
    | Cos -> cos (evaluate a x)
    | Ln -> log (evaluate a x)
    | Neg -> -1. *. (evaluate a x))
;;




(*>* Problem 2.3 [20 POINTS] *>*)
(* See writeup for instructions.  *)
let rec derivative (e:expression) : expression =
  match e with 
  | Num x -> Num (0.)
  | Var -> Num (1.)
  | Binop (op, a, b) -> (
    match op with 
    | Add -> Binop (Add, (derivative a), (derivative b))
    | Sub -> Binop (Sub, (derivative a), (derivative b))
    | Mul -> Binop (Add, Binop (Mul, (derivative a), b), Binop (Mul, a, (derivative b)))
    | Div -> Binop (Div, Binop (Sub, Binop (Mul, b, (derivative a)), Binop (Mul, a, (derivative b))), Binop (Pow, b, Num (2.))) 
    | Pow -> (
      match b with
      | Num _ -> Binop (Mul, Binop (Mul, b, Binop (Pow, a, Binop (Sub, b, Num(1.)))), (derivative a))
      | Var | Binop _ | Unop _ -> Binop (Mul, Binop (Pow, a, b), Binop (Add, Binop (Mul, (derivative b), Unop(Ln, a)), Binop (Div, Binop (Mul, (derivative a), b), a)))))
  | Unop (op, a) -> (
    match op with
    | Sin -> Binop (Mul, (derivative a), Unop (Cos, a))
    | Cos -> Binop (Mul, (derivative a), Unop (Neg, Unop (Sin, a)))
    | Ln ->  Binop (Div, (derivative a), a)
    | Neg -> Unop (Neg, (derivative a)))
;;





(* A helpful function for testing. See the writeup. *)
let checkexp strs xval=
  print_string ("-------------------------------\n");
  print_string ("Checking expression: " ^ strs^"\n");
  let parsed = parse strs in (
    print_string "contains variable: ";
    print_string (string_of_bool (contains_var parsed));
    print_endline " ";
    print_string "Result of evaluation: ";
    print_float  (evaluate parsed xval);
    print_endline " ";
    print_string "Result of derivative: ";
    print_endline " ";
    print_string (to_string_smart (derivative parsed));
    print_endline " ")
;;

(* === Testing Cases === *)
checkexp "x" 2.0;;
(* Come up your own test cases here. *)





(*>* Problem 2.4 [10 POINTS] *>*)
(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int) : float option =
  if (((evaluate e g) < epsilon) && ((evaluate e g) > (-1. *. epsilon)))
  then Some g
  else
    match (g, lim) with
    | (_, 0) -> Some g
    | (_, _) -> find_zero e (g -. ((evaluate e g) /. (evaluate (derivative e) g))) epsilon (lim - 1)
;;



(*>* Problem 2.5 [30 POINTS] *>*)
(* See writeup for instructions. *)
(* For this problem, we assume the input expression ONLY contains
 * Add, Sub, Mul, and Neg operators (nested arbitrarily). *)
let rec find_zero_exact (e:expression) : expression option =
  let join_polynomials (list1 : (float * int) list) 
        (list2 : (float * int) list) : (float * int) list =
    match (list1, list2) with
      | ([], []) -> []
      | (xs, []) -> []
      | ([], xs) -> []
      | (xs, ys) -> xs @ ys
  in
  let rec generate_polynomial 
            (curr_exp : expression) (prev_poly: (float * int) list) : (float * int) list = 
    match (curr_exp, prev_poly) with
      | (_, []) -> []
      | (Num a, _) -> List.map (fun (coeff, pow) -> (coeff *. a, pow)) prev_poly
      | (Var, _) -> List.map (fun (coeff, pow) -> (coeff, pow + 1)) prev_poly
      | (Binop (op, a, b), _) -> (
          match op with
            | Add -> join_polynomials (generate_polynomial a prev_poly) 
                       (generate_polynomial b prev_poly)
            | Sub -> join_polynomials (generate_polynomial a prev_poly) 
                       (generate_polynomial b (List.map (fun (coeff, pow) -> (-1. *. coeff, pow)) prev_poly))
            | Mul -> (generate_polynomial b (generate_polynomial a prev_poly))
            | Div | Pow -> []
        )
      | (Unop (op, a), _) -> (
          match op with
            | Neg -> 
                generate_polynomial a (List.map (fun (coeff, pow) -> (-1. *. coeff, pow)) prev_poly)
            | Cos | Sin | Ln -> []
        )
  in
  let rec collect_terms 
            (prev_list : (float * int) list) (new_element : float * int) : (float * int) list = 
    let (new_coeff, new_pow) = new_element in
      match prev_list with
        | [] -> [new_element]
        | (curr_coeff, curr_pow) :: tl -> (
            if (curr_pow = new_pow)
            then (curr_coeff +. new_coeff, curr_pow) :: tl
            else
            if (curr_coeff = 0.)
            then collect_terms tl new_element
            else (curr_coeff, curr_pow) :: collect_terms tl new_element
          )
  in
  let rec remove_zero_coefficients 
            (input : (float * int) list) : (float * int) list = 
    match input with
      | [] -> [] 
      | (curr_coeff, curr_pow) :: tl -> (
          if (curr_coeff = 0.)
          then remove_zero_coefficients tl
          else (curr_coeff, curr_pow) :: remove_zero_coefficients tl
        )
  in
  let rec check_order (order : int) (input: (float * int) list) : bool = 
    match input with
      | [] -> true
      | (curr_coeff, curr_pow) :: tl -> (
          if (curr_pow > order)
          then false
          else check_order order tl
        )
  in
  let solve (input : (float * int) list) : expression option = 
    match input with
      | [(b, 0); (a, 1)] -> Some (Num (-1. *. b /. a))
      | [(a, 1); (b, 0)] -> Some (Num (-1. *. b /. a))
      | [(a, 1);] -> Some (Num 0.)
      | [(b, 0);] -> None
      | [] -> None
      | _ -> None
  in
    match e with
      | Num x -> Some (Num x)
      | Var -> Some (Num 0.)
      | Binop (_, _, _) | Unop (_, _) -> (

          let polynomial = generate_polynomial e [(1., 0)] in
            match polynomial with
              | [] -> None
              | [(0., _)] -> None
              | x -> (

                  let simplified_poly = 
                    remove_zero_coefficients (List.fold_left (collect_terms) [] x) in
                    if (check_order 1 simplified_poly)

                    then (solve simplified_poly)

                    else None
                )
        )
;;





(* A helpful function to test finding zeros. See the writeup. *)
let check_zero str=
  print_string ("-------------------------------\n");
  print_string ("Checking zeros of " ^ str ^"\n");
  let parsed = parse str in (
    print_string "find_zero (initial guess 1.0, epsilon 0.0001, lim 100): ";
    let z = find_zero parsed 1.0 0.0001 100 in (
      match z with
        | None -> print_string "None"
        | Some f -> print_string (string_of_float f) );
      print_endline " ";
      print_string "find_zero_exact: ";
      let z' = find_zero_exact parsed in (
        match z' with
          | None -> print_string "None"
          | Some e -> print_string (to_string e) );
        print_endline " ")
;;

(* === Testing Cases === *)
check_zero "x";;
(* Come up your own test cases here. *)






(*>* [OPTIONAL] *>*)
(* For extra fun (but not extra credit),
   implement find_zero_exact_2 that solves degree-two expressions.
   This is almost as easy as solving degree-one expressions,
   if you use the quadratic formula.  Almost as easy, assuming
   you've already done the work to normalize polynomials into an
   easily recognizable form. *)
(*
let rec find_zero_exact_2 (e:expression) : string =
unimplemented "find_zero_exact_2"
;;
*)


(* A helpful function to test finding zeros (accept 2-degree expression). *)
(*
let check_zero_2 str=
print_string ("-------------------------------\n");
print_string ("Checking zeros (accept 2-degree expression) of " ^ str ^"\n");
let parsed = parse str in (
print_string "find_zero (initial guess 1.0, epsilon 0.0001, lim 100): ";
let z = find_zero parsed 1.0 0.0001 100 in (
match z with
| None -> print_string "None"
| Some f -> print_string (string_of_float f) );
print_endline " ";
print_string "find_zero_exact_2: \n";
print_string (find_zero_exact_2 parsed);
print_endline " ")
;;
*)


(* === Testing Cases === *)
(* You may come up with your own testging cases. *)
(* check_zero_2 "x";; *)
(* Come up your own test cases here. *)






(*>* [OPTIONAL] *>*)
(* For extra fun (but not extra credit), 

   Consider this function,
   let evaluate2 (e: expression) : float -> float =
   let e' = derivative e in
   fun x -> (evaluate e x, evaluate e' x)

   Such a function can be used in Newton's method.
   But if the expression e is large, e' can be exponentially larger,
   because of the chain rule for multiplication, so
   evaluate e' x  can be slow.

   One solution is called "forward mode automatic differentiation",
   which has become an important algorithm (since 2017 or so) in
   deep learning.  You can read about it in section 3.1 of
   this paper:  http://jmlr.org/papers/volume18/17-468/17-468.pdf
   "Automatic Differentiation in Machine Learning: A Survey"
   (and pay particular attention to Table 2 for a worked example).

   So, the challenge (which is actually not very difficult) is,
   write this function

   let evaluate2 (e: expression) (x: float) : float * float = ...

   that computes both e(x) and the first derivative e'(x),
   without ever calculating (derivative e).  Like evaluate,
   do it by case analysis on the syntax-tree of e. *)
	   
(* Q.  Why do it, if no extra credit?
   A.  Because (and only if) it's fun.  
   A.  Because CS5035 is a graduate course.
   A.  Because the main reason you're working so hard
   is to learn things, not just to get grades.
   A.  Any well educated computer scientist graduating after 2019
   ought to know something about deep learning . . .
*)




