(*********************)
(* Dynamic ML Syntax *)
(*********************)

type variable = string 

(* Equality and Inequality for variables *)
let var_eq x y = (String.compare x y = 0)
let var_neq x y = not (String.compare x y = 0)

type constant = Int of int | Bool of bool 

type operator = Plus | Minus | Times | Div | Less | LessEq 

type exp = 

  (* Basic *)
  | Var of variable   
  | Constant of constant
  | Op of exp * operator * exp
  (* If (conditional, consequent, alternative) *)
  | If of exp * exp * exp
  (* Let (v, e1, e2) is a let binding with the form let v = e1 in e2 *)
  | Let of variable * exp * exp

  (* Pairs *)
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp

  (* Lists *)
  | EmptyList
  (* Note 1: this permits type-heterogeneity, unlike OCaml, e.g.:
     Cons (Constant (Int 1), Cons (Constant (Bool true), EmptyList)) 
    
     Note 2: this also doesn't enforce that a list should end in a 
     Cons with EmptyList as the second argument, but we will only 
     ever use well-defined lists of this structure.
  *)
  | Cons of exp * exp
  (*   Match (e1, e2, hd, tl, e3) is a match statement with the form:
   match e1 with 
   | [] -> e2 
   | hd::tl -> e3 
  *)                  
  | Match of exp * exp * variable * variable * exp  

  (* Functions *)
  (* Rec (f, x, b) is the definition of a possibly-recursive function
     The function is named f and x is the name of the parameter. b is 
     the body of the expression, and may contain f and/or x.
  *)
  | Rec of variable * variable * exp
  (*
   Closure (e, f, x, b) is a closure of a possibly-recursive function.
   The closure environment is e.  f, x, and b are the same as in the 
   definition of the function.
  *)
  | Closure of env * variable * variable * exp
  (* App (f, x) is a function call of closure f with argument x *)
  | App of exp * exp

and env = (variable * exp) list

(*****************************)
(* Manipulating environments *)
(*****************************)
 
(* empty environment *)
let empty_env : env = []

(* lookup_env env x == Some v 
 *   where (x,v) is the most recently added pair (x,v) containing x
 * lookup_env env x == None 
 *   if x does not appear in env *)
let rec lookup_env (env:env) (x:variable) : exp option =
  match env with
    [] -> None
  | (curr_var, curr_exp)::tl -> (
    if (var_eq x curr_var)
    then Some curr_exp
    else lookup_env tl x
  )
;;

(* update env x v returns a new env containing the pair (x,v) 
 * The exact operation (replacing, overriding, etc.) is up to 
 * you, but clearly your other functions (notably lookup_env) 
 * must know the semantics of update_env
*)
let update_env (env:env) (x:variable) (v:exp) : env = 
  (x, v)::env
;;