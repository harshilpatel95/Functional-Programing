open Syntax

let string_of_const c = 
  match c with 
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b


let string_of_op op = 
  match op with 
    | Plus -> "+" 
    | Minus -> "-" 
    | Times -> "*" 
    | Div -> "/" 
    | Less -> "<" 
    | LessEq -> "<=" 

let max_prec = 10

let precedence e = 
  match e with 
    | Constant _ -> 0
    | Var _ -> 0
    | Op (_,Plus,_) -> 5
    | Op (_,Minus,_) -> 5
    | Op (_,Times,_) -> 3
    | Op (_,Div,_) -> 3
    | Op (_,Less,_) -> 7
    | Op (_,LessEq,_) -> 7
    | Let _ -> max_prec
    | If _ -> max_prec

    | Pair _ -> 0
    | Fst _ -> 2
    | Snd _ -> 2

    | EmptyList -> 0
    | Cons _ -> 8
    | Match _ -> max_prec

    | Rec _ -> max_prec
    | Closure _ -> max_prec
    | App _ ->  2

let rec env2string env =
  let elem2string x v = x ^ "=" ^ exp2string max_prec v in
  let rec aux env =
    match env with
	[] -> ""
      | [(x,v)] -> elem2string x v
      | (x,v)::rest -> elem2string x v ^ ";" ^ aux rest 
  in
  "[" ^ aux env ^ "]"

and exp2string prec e = 
  let p = precedence e in 
  let s = 
    match e with 
      | Constant c -> string_of_const c
      | Op (e1,op,e2) -> 
          (exp2string p e1) ^ " "^(string_of_op op)^" "^(exp2string prec e2)
      | Var x -> x
      | If (e1, e2, e3) -> 
        "if " ^ (exp2string max_prec e1) ^ 
        " then " ^ (exp2string max_prec e2) ^ 
        " else " ^ (exp2string p e3)
      | Let (x,e1,e2) -> "let "^x^" = "^(exp2string max_prec e1)^" in "^
          (exp2string prec e2)

      | Pair (e1, e2) -> 
	  "(" ^ (exp2string max_prec e1) ^ "," ^ (exp2string max_prec e2)  ^ ")"
      | Fst e1 ->  "fst " ^ (exp2string p e1)
      | Snd e1 ->  "snd " ^ (exp2string p e1)

      | EmptyList -> "[]"
      | Cons (e1,e2) -> (exp2string p e1) ^ "::" ^ (exp2string prec e2) 
      | Match (e1,e2,hd,tl,e3) -> 
	  "match " ^ (exp2string max_prec e1) ^ 
	    " with [] -> " ^ (exp2string max_prec e2) ^ 
            " | " ^ hd ^ "::" ^ tl ^ " -> " ^ (exp2string p e3)

      | Rec (f,x,body) -> "rec "^f^" "^x^" = "^(exp2string max_prec body)
      | Closure (env,f,x,body) -> 
	  "closure "^env2string env^" "^f^" "^x^" = "^(exp2string max_prec body)
      | App (e1,e2) -> (exp2string p e1)^" "^(exp2string p e2)

  in 
    if p > prec then "(" ^ s ^ ")" else s

let string_of_exp e = exp2string max_prec e 
let string_of_env env = env2string env
