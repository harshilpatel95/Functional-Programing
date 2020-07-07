(*** CS5035 Assignment 1: Easy as Pi ***)
(*** Harshil Patel ***)

(* The assignment is due on February 12 (Wednesday) at 11:59 PM. *)

(* Submit your assignment in CSNS.
 *
 * Make sure that your submission does NOT have
 * both compiling errors and running errors. *)


(* Don't remove the following line. *)
let undefined : unit -> 'a = fun () -> failwith "undefined";;


(* Problem 1. Please define these variables with the appropriate values.
 * Be sure that these statements all type-check after editing them.
 * You can do this by compiling with "ocamlc" in the terminal emulator. *)

(* 1.a. Create a string with your first name.
 * Your code should replace undefined (). *)
let first_name : string = undefined ();;
let first_name : string = "harshil";;

(* 1.b. Use a string operator on the string from 1.a. to create 
 * a string that contains both your first and last names.
 * Your code should replace undefined (). *)
let name : string = undefined ();;
let name : string = "patel"

(* 1.c. Create a string containing your email address.
 * Your code should replace undefined (). *)
let email : string = undefined ();;
let email : string = "harshilgp@gmail.com";;

(* 1.d. Create an integer with your (estimated) graduate year.
 * Your code should replace undefined (). *)
let class_year : int = undefined ();;
let class_year : int = 2020;;

(* 1.e. Replace the ??? with what you're excited about in this course. *)
let exciting : string = "I'm excited about assignement";;

let print = Printf.sprintf;;

let survey () = 
  print "----------------------------------------\n" ^
    print "Name: %s\n" name ^
    print "Email: %s\n" email ^
    print "Year: %d\n" class_year ^
    print "%s\n" exciting ^
    print "----------------------------------------\n"
;;

print_endline (survey ());;

(* Problem 2. Fill in types:
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to type check it before submission. 
 * Note that the expressions might not do anything useful -- and in fact 
 * might even display interesting problems! -- but all you should do is fill 
 * in the ???s to make them type check. *)

(* 2.a. *)
let prob2a : string  = let greet y = "Hello " ^ y in greet "World!";;

(* 2.b. *)
let prob2b : float = float_of_int (int_of_float(2.2 +. 7.7));;

(* 2.c. *)
let rec prob2c (x : char) : char =
  prob2c (if true then prob2c x else 'h')
;;

(* 2.d. *)
let rec prob2d (y:bool) (z:bool) : bool =
  prob2d (prob2d z y) (not y)
;;




(* Problem 3. Briefly explain why each of prob3{a,b,c} will not
 * compile (use the strings exp3{a,b,c} for your answers) and
 * change the code in some small way so that it does compile.
 *
 * Do NOT change the top-level type associated with the expression. *)

(* 3.a Your code should replace undefined (). *)
let exp3a : string = "This expression has type int but an expression was expected of type float";;

let prob3a : bool = 
  let compare (x, y) = x < y in 
    compare (3.9, 4.) 
;;

(* 3.b. Your code should replace undefined (). *)
let exp3b : string = "This expression has type 'a -> 'b -> 'b but an expression was expected of type int";;

let prob3b : int = 
  let fib n =
    let rec aux n y x =
      if n <= 0 then x 
      else aux (n-1) (x+y) y 
    in
      aux n 1 0
  in
    fib 10
;;

(* 3.c. Your code should replace undefined (). *)
let exp3c : string = "Unbound value sumTo";;

let prob3c : int =
  let rec sumTo (n:int) : int =
    if n <= 0 then 0
    else n + sumTo (n-1)
  in
    sumTo 10
;;

(* Problem 4. Solve the following sub-problems.*)

(* 4.a. Fill in the ??? with an expression that uses x and y
 * and has the right type. 
 *
 * Your answer could be ANY well-typed expression.*)
 let prob4a =
  let u = 32.0 in 
  let v = 28.0 in
  let square w = w *. w in
  let boff (x) (y) = square (x*.y) in
  let d = sqrt (boff u v) in
    int_of_float d
;;


(* Also:  What warning message do you get if your ??? expression does not
 * use the function "square"? *)
(* Your code should replace undefined (). *)
let warn4a : string = "Warning 26: unused variable square.";;


(* 4.b. Replace each ??? with the type of the corresponding expression,
 * and write a function f that has the correct type signature. Explain
 * in exp4b a problem that remains with the function prob4b.
 *
 * The function f could implement any functionality. *)
 let f (a: int) (b: int) : float = float(a+b)
 ;;
 
 let rec prob4b (x: float) (y: int) : float =
   prob4b (f y 4) (int_of_float x)
 ;;

(* Your code should replace undefined (). *)
let exp4b : string = "Error: Syntax error: type expected.";;


(* 4.c. Is it possible to find types for the argument and result that
 * make the function forever type check?
 *
 * If possible, give correct types (replace ???), and comment
 * the let expression for exp4c.
 *
 * If not possible, explain why it is impossible in the string exp4c
 * (replace undefined ()), and comment the let expression for forever. *)
(*let rec forever (x:???) : ??? =
  forever forever
  ;;
*)

let exp4c : string = "Error: Syntax error: type expected.";;




(* Problem 5. *)
(* Implement the function few_divisors, which takes two
 * parameters n and m, and should return true if n has
 * fewer than m divisors (including 1 and n), return
 * false otherwise.
 *
 * The few_divisors function should call the function
 * (bad_divisors n m) defined below  if n <= 0 or m < 0.
 *
 * Some examples are as follows.
   few_divisors 17 3;; (* true -- 17 has only 1 and 17 *)
   few_divisors 4 3;;  (* false -- 4 has 1, 4, and 2 *)
   few_divisors 4 4;;  (* true -- 4 has only 1, 4, and 2 *)
   few_divisors 18 6;; (* false -- 18 has 1, 18, 2, 3, 6, and 9 *)
   few_divisors 18 7;; (* true -- 18 has only 1, 18, 2, 3, 6, and 9 *)
*)

(* Don't remove these lines. Your code should replace undefined (). *)
exception BadDivisors of int * int;;
let bad_divisors n m = raise (BadDivisors (n,m));;

let rec few_divisors (n:int) (m:int) : bool =
  undefined ()
;;

(* After the implementation, test your code with
 * following assertions.
 *
 * If your code passes all the following tests, it does NOT
 * mean that your implementation is COMPLETELY correct.
 *
 * You may figure out other test cases. *)
assert (few_divisors 17 3);;
assert (not (few_divisors 4 3));;
assert (few_divisors 4 4);;





(* Problem 6. Approximating Pi *)

exception BadArg of int;;
let bad_arg (n:int) = raise (BadArg n);;

(* 6.a. - Sinusoidal Approximation *)
(* Use the following equations to define a function sin_pi that returns 
 * the ith approximation of pi.  

 * approx(0) = 3
 * approx(n+1) = approx(n) + sin(approx(n))

 * Using this approximation, you will converge on many digits of pi very
 * fast.  The first few digits of pi are 3.14159 26535 89793 23846 26433.  
 * approx(1) accurately predicts these digits:  3.141
 * approx(2) accurately predicts these digits:  3.14159 26535
 * approx(3) accurately predicts these digits:  3.14159 26535 89793
 * 
 * sin_pi should call the pre-defined function (bad_arg n)
 * if n is less than 0 *)

(* Your code should replace undefined (). *)
let rec sin_pi (n:int) : float =
  undefined ()
;;

(* After the implementation, test your code with
 * following assertions.
 *
 * If your code passes all the following tests, it does NOT
 * mean that your implementation is COMPLETELY correct.
 *
 * You may figure out other test cases. *)
assert (sin_pi 0 = 3.0);;
assert (sin_pi 1 = 3.1411200080598674);;
assert (sin_pi 2 = 3.1415926535721956);;
assert (sin_pi 3 = 3.1415926535897931);;





(* 6.b. Monte Carlo Approximation *)
(* A Monte Carlo method relies on repeated random sampling to simulate
 * some process or compute a value.  See Wikipedia:
 * http://en.wikipedia.org/wiki/Monte_Carlo_method
 * 
 * Pi can be computed using Monte Carlo simulation through a series
 * of experiments.  Here is a single experiment:
 *
 *  -- choose a pair of random floating point numbers between 0 and 1
 *  -- call the numbers x and y 
 *  -- think of (x,y) as a point on the plane in the unit square
 *  -- test whether the point falls within the unit circle by measuring
 *     the distance from the point to the origin:  x^2 + y^2 <= 1
 *
 * Now suppose you do n experiments and in m of those experiments, the
 * random point chosen falls within the upper right quarter of the unit circle.
 * Since the area of a circle is known to be pi * r^2 and the area of
 * a square is r^2 (and here we are dealing with a radius/square side
 * of length 1), the following equations hold:

   m     quarter of area of circle     1/4 * pi * r^2
   --- = -------------------------  =  -------------- = 1/4 * pi
   n        area of square                r^2

 * Use the above information to write the function monte_pi, which 
 * takes a positive number indicating the number of random points n to
 * sample and approximates pi using that number of random points.
 * 
 * monte_pi should call the pre-defined function (bad_arg n)
 * when n is not positive.
*)

(*
* To compute some random numbers, use OCaml's Random library:
*
* http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html
* 
* We initialize the library below.
*
* (Random.float f) will return a random floating point number between 0.0 and f.
*
* Note: this estimation method will converge far more slowly than the
* sinusoidal method (because the sin function already captures pi, so
* that approximation was really cheating!).  I only had the first 2 
* digits after 5000 trials.
* I estimated pi at 3.141628 after 1,000,000 trials (your result may
* vary depending on exact details of your computation).

* =======
* WARNING:
* =======
* If you make too many recursive calls in a row, you may run
* to a stack overflow error that looks like this:

* Stack overflow during evaluation (looping recursion)?

* Do not worry about that message -- just try your code on fewer trials.
* You don't necessarily have to be able to execute 1,000,000.  For example,
* if you code works on 100 or 1000 recursive calls, that is just fine.
*
* "Too many calls" may vary on different machines. Later in the semester
* we will discuss "tail recursion" and how to fix this problem.
*
*)

(* Don't remove these lines. Your code should replace undefined (). *)
Random.init 17;; (* Don't remove this line; Your code follows *)

type point = { x: float; y: float }

let monti_pi (times: int): float = 
  let rec run (times:int) (within: int) = match times with
    | 0 -> within
    | t -> 
        let p = { x = Random.float 1.; y = Random.float 1. } in
        let distance {x = x; y = y} = sqrt (x *. x +. y *. y) in
        if distance p <= 1. 
        then (run (times - 1) (within + 1))
        else (run (times - 1) within)
  in
  let n = run times 0 in
  (Float.of_int n) *. 4. /. (Float.of_int times)
;;






(* Problem 7. *)
(* Look up another technique for approximating pi on the web.
 * As a starting point, see here:  
 *
 * http://en.wikipedia.org/wiki/Approximations_of_%CF%80
 *
 * You might be able to find other interesting articles on the web too.
 * 
 * The algorithm you choose must be capable of computing many digits of
 * pi.  Algorithms that compute just 1 approximation (such as 3 or
 * 3927/1250 or any other fixed fraction) are insufficient.  Choose 
 * an algorithm that successively approximates pi in some manner.  
 * Your algorithm may not use trigonometric functions such as sin, 
 * cos, arctan, etc.
 *
*)

(* 7.a. Briefly explain your algorithm and your sources in the string exp7a. *)
(* Your code should replace undefined (). *)
let exp7a : string = undefined ();;


(* 7.b. Implement your algorithm, i.e. the your_pi function.
 * 
 * your_pi should take a positive integer parameter n which
 * increases the precision of your approximation as it increases.
 *
 * your_pi should call the function (bad_arg n) defined in Problem 6
 * when n is not positive.
 * 
 * Explain what the parameter is used for in your algorithm
 * in the string exp7b. After the implementation, show some test cases.

 * Again, don't worry about stack overflow errors for large values
 * of the input to your_pi.
*)

(* Your code should replace undefined (). *)
let exp7b : string = undefined ();;

let rec your_pi (n:int) : float =
  undefined ()
;;

(* After the implementation, figure out at least three test cases
 * excluding non-positive arguments. *)
(* Your code should replace ??? *)
assert (your_pi ??? = ???);;
assert (your_pi ??? = ???);;
assert (your_pi ??? = ???);;
(* MORE TEST CASES *)


