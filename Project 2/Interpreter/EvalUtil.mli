(***************************************)
(* Utilities Shared Between Evaluators *)
(***************************************)

open Syntax

(* Error conditions *)
exception UnboundVariable of variable 
exception BadApplication of exp 
exception BadIf of exp 
exception BadMatch of exp 
exception BadOp of exp * operator * exp 
exception BadPair of exp 

(* evaluation of primitive operators *)
val apply_op : exp -> operator -> exp -> exp
