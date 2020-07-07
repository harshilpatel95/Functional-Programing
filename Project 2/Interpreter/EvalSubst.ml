(*************************************************)
(* A substitution-based evaluator for Dynamic ML *)
(*************************************************)

open Syntax
open Printing
open EvalUtil

(* closures should not be used in a substitution-based interpreter *)
exception NoClosures of exp

(* the subset of expressions considered values *)
let rec is_value (e:exp) : bool =
  match e with
      Constant _ -> true  
    | Var x -> raise (UnboundVariable x)  
    | Closure _ -> raise (NoClosures e)   
    | Op (e1,op,e2) -> false
    | If (e1,e2,e3) -> false
    | Let (y,e1,e2) -> false
    | Rec (f,y,e1) -> true
    | App (e1,e2) -> false
    | _ -> failwith "unimplemented"

(* Note that the scope of the function name f includes the body of 
 * of the function. Hence, we test for both x = f and x = y when
 * substituting in to a recursive function.
 *)
let substitute (v:exp) (x:variable) (e:exp) : exp = 
  let rec subst (e:exp) : exp = 
    match e with 
    | Var y -> if x = y then v else e
    | Constant _ -> e
    | Op (e1,op,e2) -> Op(subst e1,op,subst e2)
    | If (e1,e2,e3) -> If(subst e1,subst e2,subst e3)
    | Let (y,e1,e2) -> 
        Let (y, subst e1, if x = y then e2 else subst e2)
    | Pair (e1,e2) -> Pair(subst e1, subst e2)
    | Fst e1 -> Fst (subst e1)
    | Snd e1 -> Snd (subst e1)
    | EmptyList -> EmptyList
    | Cons (e1,e2) -> Cons (subst e1, subst e2)
    | Match (e1,e2,hd,tl,e3) -> 
      Match (subst e1, subst e2, hd, tl,
             if x = hd || x = tl then e3 else subst e3)
    | Rec (f,y,e1) -> if x = f || x = y then e else Rec (f, y, subst e1)
    | App (e1,e2) -> App(subst e1,subst e2)
    | Closure _ -> raise (NoClosures e) 
  in 
    subst e


let eval_body (eval_loop:exp->exp) (e:exp) : exp = 
  match e with
    | Constant _ -> e  
    | Op (e1,op,e2) -> 
        let v1 = eval_loop e1 in 
        let v2 = eval_loop e2 in 
          apply_op v1 op v2 
    | If (e1,e2,e3) -> 
          (match eval_loop e1 with 
             | Constant (Bool true) -> eval_loop e2
             | Constant (Bool false) -> eval_loop e3
             | v1 -> raise (BadIf v1))
    | Let (x,e1,e2) -> eval_loop (substitute (eval_loop e1) x e2)

(* STUDENTS:  YOU DO ****NOT**** HAVE TO IMPLEMENT THIS. *)

    | Pair (e1,e2) ->  failwith "unimplemented"
    | Fst e1 ->  failwith "unimplemented"
    | Snd e1 ->  failwith "unimplemented"
    | EmptyList ->  failwith "unimplemented"
    | Cons(e1,e2) ->  failwith "unimplemented"
    | Match(e1,e2,hd,tl,e3) ->  failwith "unimplemented"

    | Rec _ -> e
    | App (e1,e2) -> 
        (match eval_loop e1 with 
	   | (Rec (f,x,body)) as recf ->
	     let v2 = eval_loop e2 in
	     let body' = substitute v2 x body in
	     let body'' = substitute recf f body' in 
	     eval_loop body''
           | v1 -> raise (BadApplication v1))

    | Var x -> raise (UnboundVariable x)

    | Closure _ -> raise (NoClosures e)

(* In this version of the debug_eval, we don't bother to 
 * print out the values. *)
let rec debug_eval e = 
  (if is_value e then () else 
     Printf.printf "Evaluating %s\n" (string_of_exp e)) ; 
  let v = eval_body debug_eval e in 
    (if is_value e then () else 
       Printf.printf "%s evaluated to %s\n" 
         (string_of_exp e) (string_of_exp v)) ; 
    v

let rec eval e = eval_body eval e
