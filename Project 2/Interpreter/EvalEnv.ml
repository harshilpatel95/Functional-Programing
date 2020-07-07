(*************************************************)
(* An environment-based evaluator for Dynamic ML *)
(*************************************************)

open Syntax
open Printing
open EvalUtil

(* Defines the subset of expressions considered values
   Notice that closures are values but the rec form is not -- this is
   slightly different from the way values are defined in the 
   substitution-based interpreter.  Rhetorical question:  Why is that?
   Notice also that Cons(v1,v2) is a value (if v1 and v2 are both values).
*) 
let rec is_value (e:exp) : bool = 
  match e with
  Constant _ -> true
  | Var x -> false
  | Op _ -> false
  | If _ -> false
  | Let _ -> false
  | Pair _ -> false
  | Fst x -> false
  | Snd x -> false
  | EmptyList -> true
  | Cons _ -> false
  | Match _ -> false
  | Rec _ -> false
  | Closure _ -> true
  | App _ -> false

;;
    

(* evaluation; use eval_loop to recursively evaluate subexpressions *)
let eval_body (env:env) (eval_loop:env -> exp -> exp) (e:exp) : exp = 
  match e with
  | Constant x -> e
  | Var x -> 
    (
    match lookup_env env x with 
    | None -> raise (UnboundVariable x)
    | Some v -> v)
  | Op (e1, op, e2) ->
    ( 
      let v1 = eval_loop env e1 in
      let v2 = eval_loop env e2 in
      apply_op v1 op v2
    )
  | If (e1, e2, e3) -> 
    (match eval_loop env e1 with
    | Constant (Bool true) -> eval_loop env e2
    | Constant (Bool false) -> eval_loop env e3
    | v1 -> raise (BadIf v1))
  | Let (x, e1, e2) -> 
    (eval_loop (update_env env x (eval_loop env e1)) e2)
  | Pair (e1, e2) -> Pair((eval_loop env e1), (eval_loop env e2))
  | Fst (e1) -> (
    match eval_loop env e1 with
    | Pair (a, b) -> (eval_loop env a) 
    | _ -> Fst (eval_loop env e1))
  | Snd (e1) -> (
    match eval_loop env e1 with
    | Pair (a, b) -> (eval_loop env b)
    | _ -> Snd (eval_loop env e1)) 
  | EmptyList -> e
  | Cons (e1, e2) -> Cons ((eval_loop env e1), (eval_loop env e2))
  | Match (e1, e2, hd_var, tl_var, e3) ->
    (
      match eval_loop env e1 with
      | EmptyList -> eval_loop env e2
      | Cons(hd, tl) -> 
	(
	  let hd_env = update_env env hd_var (hd) in
	  let hd_tl_env =  update_env hd_env tl_var (tl) in
	  eval_loop hd_tl_env e3
	)
      | _ -> raise (BadMatch e1)
    )
  | Rec (f, x, body) -> 
    (
      let rec find_bound_vars (exp:exp) : variable list = 
	match exp with
	| Var (e1) -> []
	| Constant (e1) -> []
	| Op (e1, op, e2) -> find_bound_vars e1 @ find_bound_vars e2
	| If (e1, e2, e3) -> find_bound_vars e1 @ find_bound_vars e2 @ find_bound_vars e3 
	| Let (var, e1, e2) -> var :: find_bound_vars e1 @ find_bound_vars e2
	| Pair (e1, e2) -> find_bound_vars e1 @ find_bound_vars e2
	| Fst (e1) -> find_bound_vars e1
	| Snd (e1) -> find_bound_vars e1
	| EmptyList -> []
	| Cons(e1, e2) -> find_bound_vars e1 @ find_bound_vars e2
	| Match (e1, e2, hd_var, tl_var, e3) -> hd_var :: tl_var :: find_bound_vars e1 @ find_bound_vars e2 @ find_bound_vars e3
	| Closure (env, f, x, body) -> f :: x :: find_bound_vars body
	| Rec (f, x, body) -> f :: x :: find_bound_vars body
	| App (e1, e2) -> find_bound_vars e1 @ find_bound_vars e2
      in
      let rec get_free_vars (env:env) (bound_list: variable list) : env = 
	match env with 
	| [] -> []
	| (curr_var, curr_exp) :: tl -> 
	  (if (List.filter (fun x -> (var_eq x curr_var)) bound_list) = [] 
	   then (curr_var, curr_exp) :: get_free_vars (tl) (bound_list)
	   else get_free_vars (tl) (bound_list))
      in
      Closure (get_free_vars env (find_bound_vars body), f, x, body)
    ) 
  | App (e1, e2) -> 
    (
      match eval_loop env e1 with
      | (Closure (closure_env, f, x, body)) as g ->
	(
	  let x_env = update_env closure_env x (eval_loop env e2) in
	  let f_x_env = update_env x_env f g in
	  eval_loop f_x_env body
	)
      | _ -> raise (BadApplication e1)
    )
  | Closure (closure_env, f, x, body) -> e
;;

(* evaluate closed, top-level expression e *)

let eval e =
  let rec loop env e = eval_body env loop e in
  loop empty_env e


(* print out subexpression after each step of evaluation *)
let debug_eval e = 
  let rec loop env e =
    if is_value e then e  (* don't print values *)
    else 
      begin
	Printf.printf "Evaluating %s\n" (string_of_exp e); 
	let v = eval_body env loop e in 
	Printf.printf 
	  "%s evaluated to %s\n" (string_of_exp e) (string_of_exp v); 
	v
      end
  in
  loop empty_env e
