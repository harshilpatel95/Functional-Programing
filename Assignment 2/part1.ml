(*** CS5035 Assignment 2 Part 1: Lists [100 POINTS] ***)

(* The assignment is due on February 26 (Wednesday) at 11:59 PM. *)

(* Zip all files and submit in CSNS.
 *
 * Make sure that your submission does NOT have
 * both compiling errors and running errors. *)

(* DO NOT USE MAP/REDUCE FOR THIS ASSIGNMENT!!! *)
(* Harshil Patel cin-306646748*)


(* Don't remove the following line. *)
let undefined (s:string) : 'a = failwith (s^" undefined");;
let unimplemented (s:string) : 'a = failwith (s^" unimplemented");;




(*************)
(* PROBLEM 1 [15 POINTS] *)
(*************)
(* For each part of problem 1, explain in the given string why the code
   will not typecheck, then follow the instructions for that part to
   change the code so that it typechecks while keeping the same values
   as intended by the erroneous code.
*)

(* Problem 1a [5 POINTS] - Give your explanation in exp1a and then fix the
   right-hand-side of the assignment to match the listed type. 
   (Do not change the left-hand-side.)
*)
let exp1a : string = "This expression has type 'a * 'b * 'c
       but an expression was expected of type in";;
let prob1a : int list = [1; 2; 3];;

(* Problem 1b [5 POINTS] - Give your explanation in exp1b and then fix the type
   of variable prob1b to match the type of the expression on the
   right-hand-side of the assignment. (Do not change the
   right-hand-side.)
*)
let exp1b : string = "This expression has type 'a list
       but an expression was expected of type string * int list";;
let prob1b : (string * int) list = [("CS", 5035); ("CS", 3035)];;

(* Problem 1c [5 POINTS] - Give your explanation in exp1c and then fix the
   right-hand-side of the expression to match the variable prob1c's
   listed type. 
   (Do not change the left-hand-side.)  
*)
let exp1c : string = "This expression has type 'a list
       but an expression was expected of type float";;
let prob1c : float list = 2.0 :: 3.0 :: [4.0; 5.0];;




(*************)
(* PROBLEM 2 [20 POINTS] *)
(*************)

(* Fill in expressions to satisfy the following types: 
 *
 * NOTE: for option, list, and function types, you must 
 * provide a nontrivial answer. For a list that means a 
 * non-empty one; for an option type that means a Some 
 * construction; and for a function, that means using 
 * its arguments to generate the return value.
 * example problems:
 *   let x : int option = ???
 *   let y : int list = ???
 *   let f (x: int) (y: int) : int = ???
 * incorrect answers:
 *   let x : int option = None
 *   let y : int list = []
 *   let f (x: int) (y: int) : int = 7
 * possible correct answers:
 *   let x : int option = Some 1
 *   let y : int list = [1]
 *   let y : int list = [1; 2]
 *   let y : int list = 1 :: [2]
 *   let f (x: int) (y: int) : int = x + y
 *   let f (x: int) (y: int) : int = 
 *         String.length  ((string_of_int x) ^ (string_of_int y))
*)

(*>* Problem 2a [5 POINTS] *>*)
let prob2a : (float * (string * int) option list) list =
  [(4., [Some ("hey", 4)] )]
;;

(*>* Problem 2b [5 POINTS] *>*)
(* a student is a (name, age option) pair *)
type student = string * int option;;
let prob2b : (student list option * int) list = 
  [(Some [("Harshil", Some 95)], 100)]
;;

(*>* Problem 2c [5 POINTS] *>*)
let prob2c : (int * int -> int) * (float -> float -> unit) * bool  = 
  ((fun (a, b) -> a+b), (fun a b -> print_float (a +. b)), true)
;;

(*>* Problem 2d [5 POINTS] *>*)
(* Fill in ??? with something to make prob2d typecheck *)
let prob2d =
  let rec foo bar =
    match bar with
      | (a, (b, c)) :: xs -> if a then (b + c + (foo xs)) else foo xs
      | _ -> 0
  in
    foo [(true, (5, 4)); (false, (2, 7))]
;;




(*************)
(* PROBLEM 3 [10 POINTS] *)
(*************)

(* Consider the following terribly written function: *)
let rec zardoz f ls acc =
  if (((List.length (ls@[])) = 1) = true) then (f (List.hd(ls)) (acc))
  else if (((List.length ls) = 0) = true) then acc
  else
    let hd = List.hd(ls) in
    let tl = List.tl(ls) in
    let ans = f (hd) (acc) in
    let ans = zardoz f tl ans in
      ans
;;

(* Rewrite the code above so that it does the same thing
 * but style-wise is far superior.  
 * Be sure to provide types for the function's arguments and to 
 * call itself (myzardoz NOT the original zardoz) recursively as needed.
 * You may want to write some assert statements
 * to check that your function is doing the same thing as zardoz.  
 * Use the style guide (http://cs3.calstatela.edu/~cguo/CS5035/OCaml_Style_Guide.html). *)
 let rec myzardoz f ls acc = 
  match ls with
    [] -> acc
  | hd :: [] -> f hd acc
  | hd :: tl -> myzardoz f tl (f (hd) (acc))
;;

let f (x : int) (y : float) : float = float_of_int(x) +. y;;
let ls = [4; 5; 6];;
let acc = 0.2;;

assert ((zardoz f ls acc) = (myzardoz f ls acc));;




(*************)
(* PROBLEM 4 [15 POINTS] *)
(*************)

(***************************************)
(* Conway's Lost Cosmological Theorem! *)
(***************************************)

(* 
If l is any list of integers, the look-and-say list of l is obtained by 
reading off adjacent groups of identical elements in l. For example, the 
look-and-say list of

l = [2; 2; 2]

is

[3; 2]

because l is exactly "three twos". Similarly, the look-and-say sequence of

l = [1; 2; 2]

is

[1; 1; 2; 2]

because l is exactly "one ones, then two twos".

You will now define a function look_and_say that computes the 
look-and-say sequence of its argument. look_and_say of an empty 
list is the empty list. 

For full credit your solution should be a LINEAR TIME solution.

==================================== 
CULTURAL ASIDE:

The title of this problem comes from a theorem about the sequence generated 
by repeated applications of the "look and say" operation. As look and say 
has type int list -> int list, the function can be applied to its own result. 
For example, if we start with the list of length one consisting of just the 
number 1, we get the following first 6 elements of the sequence:

[1]
[1,1]
[2,1]
[1,2,1,1]
[1,1,1,2,2,1]
[3,1,2,2,1,1]

Conway's theorem states that any element of this sequence will "decay" 
(by repeated applications of look and say) into a "compound" made up of 
combinations of "primitive elements" (there are 92 of them, plus 2 
infinite families) in 24 steps. If you are interested in this sequence, 
you may wish to consult [Conway(1987)] or other papers about the 
"look and say" operation.

==================================== 
===YOU ARE NOT REQUIRED TO IMPLEMENT THE FUNCTION RUNS!!!===
Progamming practice aside related to the "look and say" problem. You
may find this useful for constructing your solution to "look and say",
or you may not. 

Another interesting list problem is determining "runs"
in a list: maximal length sublists with all equal elements. For
example,

[1; 1; 1] and [5]

are both runs of the list

[1; 1; 1; 5; 2]

but

[1; 1] and [5; 2] and [1; 2]

are not: 

[1; 1] is not maximal
[5; 2] has unequal elements
[1; 2] is not a sublist.
*)

let look_and_say (xs: int list) : int list = 
  let rec count (input : int list) (counter : int) : int list = 
    match input with
      [] -> []
    | hd :: tl -> 
      ( match tl with 
	[] -> counter :: [hd]
      | tl_hd :: tl_tl -> if (hd = tl_hd) then count tl (counter + 1) else (counter :: hd :: (count tl 1)))
  in
  count xs 1
;;

assert (look_and_say [] = []);;
assert (look_and_say [2;2;2] = [3;2]);;
assert (look_and_say [1;2;2] = [1;1;2;2]);;
assert (look_and_say [1;2;2;1] = [1;1;2;2;1;1]);;




(*************)
(* PROBLEM 5 [10 POINTS] *)
(*************)

(* Write a function that flattens a list of lists in to a single
 * list with all of the elements in the same order they appeared in 
 * the original list of lists. eg:
 *
 * flatten [[1;2;3]; []; [4]; [5;6]] = [1;2;3;4;5;6] 
 * flatten [[]; ['e';'d']; ['a';'b';'c']] = ['e';'d';'a';'b';'c'] 
*)

let rec flatten (xss:'a list list) : 'a list =
  match xss with 
    [] -> []
  | hd :: tl -> (hd) @ (flatten tl)

;;

assert (flatten [[]] = []);;
assert (flatten [[1;2;3]; []; [4]; [5;6]] = [1;2;3;4;5;6]);;
assert (flatten [[]; ['e';'d']; ['a';'b';'c']] = ['e';'d';'a';'b';'c']);;




(*************)
(* PROBLEM 6 [30 POINTS] *)
(*************)

(* [25 POINTS]
   Return the list of all permutations of the input list. eg: 
   perm [1;2;3] = [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]] 
   The ordering of the permutations does not matter in your solution.
   We will accept either [[]] or [] as the result for perm [].
   NB: test this on small inputs - perm is ~n! which is approximately ~n^n.
*)


let perm (items:'a list) : 'a list list =
  let rec generate (item: 'a) (prev_list: 'a list) (post_list: 'a list) (perm_list: 'a list list) : 'a list list = 
    match (prev_list, post_list) with 
      ([], []) -> [[item]]
    |  (_, []) -> perm_list @ [prev_list @ [item]]
    |  ([], hd :: tl) -> perm_list @ [item :: post_list] @ (generate item [hd] tl perm_list)
    |  (_,  post_hd :: post_tl) -> perm_list @ [prev_list @ [item] @ post_list] @ (generate item (prev_list @ [post_hd]) (post_tl) perm_list)
  in let rec insert_over_nested_list (item: 'a) (nested_list: 'a list list) : 'a list list = 
  match nested_list with
    [] -> []
  | hd :: tl -> (generate item [] hd []) @ (insert_over_nested_list item tl)
in
match items with
  [] -> []
| hd :: [] -> [[hd]]
| hd :: tl -> insert_over_nested_list hd (perm tl)
;;


(* [5 POINTS]
   Define the factorial function, then try pre-defined testperm.
   This question is to verify perm is ~n!
*)
let rec fac (x:int) : int =
  unimplemented "fac"
;;
let testperm al =
  assert (List.length (perm al) = fac (List.length al))
;;
testperm [1;2;3];;

