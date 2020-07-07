(* Parsing and printing movie files *)

type movie = Query.movie;;
type studio_gross = Query.studio_gross;;

(*************)
(* Utilities *)
(*************)

(* reverse a list *)
let reverse (l:'a list) : 'a list =
  let rec aux l r = 
    match l with
      | [] -> r
      | hd::tail -> aux tail (hd::r) 
  in 
    aux l []
;;

let sep : string = "|";;

(************)
(* Printing *)
(************)

(* print movie on a line *)
let print_movie ((title, studio, gross, year):movie) : unit =
  print_string title;
  print_string sep;
  print_string studio;
  print_string sep;
  print_float gross;
  print_string sep;
  print_int year;
  print_newline ()
;;

(* print a studio, gross pair on a line *)
let print_studio ((s,g):studio_gross) : unit = 
  print_string (s ^ "|" ^ string_of_float g ^ "\n")
;;

(* print all items in a list using a custom printer *)
let rec print_many (printer:'a -> unit) (ms:'a list) : unit =
  match ms with
    | [] -> ()
    | m::rest -> printer m; print_many printer rest
;;

let rec print_lines (ss:string list) : unit =
  print_many print_endline ss
;;

let rec print_movies (ms:movie list) : unit =
  print_many print_movie ms
;;

let rec print_studios (sgs:studio_gross list) : unit =
  print_many print_studio sgs
;;

(***********)
(* Parsing *)
(***********)

(* parse movie from stdin *)
let parse_movie () : movie option =
  let line = read_line() in
    match Str.split (Str.regexp "[|\n\r\t]") line with
      | t::s::m::y::_ -> Some (t, s, float_of_string m, int_of_string y)
      | _ -> None
;;

(* parse extended movie format from stdin *)
let parse_alt_movie () : movie option =
  let line = read_line() in
    match Str.split (Str.regexp "[|\n\r\t]") line with
      | _::t::s::m::_::y::_ -> Some (t, s, float_of_string m, int_of_string y)
      | _ -> None
;;

(* parse studio from stdin *)
let parse_studio () : studio_gross option =
  let line = read_line() in
    match Str.split (Str.regexp "[|\n\r\t]") line with
      | s::g::_ -> Some (s, float_of_string g)
      | _ -> None
;;

(* parse file with one item per line *)
let parse_many (parser:unit->'a option) : 'a list =
  let all = 
    let r : 'a list ref = ref [] in
      try
        while true do
          	match parser() with
              	    Some m -> r :=  m :: !r
            	  | None -> ()  (* skip badly formatted movie *)
        done; !r
      with End_of_file -> !r
  in
    reverse all
;;

let parse_lines () : string list = parse_many (fun () -> Some (read_line()));;
let parse_movies () : movie list  = parse_many parse_movie;;
let parse_studios () : studio_gross list = parse_many parse_studio;;
let parse_alt_movies () : movie list  = parse_many parse_alt_movie;;

