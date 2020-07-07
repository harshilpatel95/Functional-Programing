(* Driver for Box Office Trivia Assignment *)
(* Contents:
   -- scripting functions over movie data (uses Query module)
   -- main driver program
*)

open Query;;
open Io;;

let debug s = print_string s; flush_all();;

(***********************)
(* Scripting Functions *)
(***********************)

(* catch bad argument exceptions and exit gracefully *)
let handler (f:'a -> 'b) (x:'a) : 'b = 
  try f x 
  with Bad_arg s -> (prerr_string s; exit 1)
;;

(* parse movies to serve as input to f; print results *)
let wrap_movies (f:movie list -> movie list) : unit = 
  print_movies (handler f (parse_movies ()))
;;

(* parse lines to serve as input to f; print results *)
let wrap_lines (f:string list -> string list) : unit = 
  print_lines (handler f (parse_lines ()))
;;

(* parse by studio format to serve as input to f; print results *)
let wrap_studios (f:studio_gross list -> studio_gross list) : unit = 
  print_studios (handler f (parse_studios ()))
;;

let do_echo () : unit = wrap_movies (fun m -> m);;

let do_decade (n:int) : unit = wrap_movies (decade n);;
let do_drop   (n:int) : unit = wrap_lines (drop n);;
let do_take   (n:int) : unit = wrap_lines (take n);;

let do_average () : unit =
  let average = string_of_float (average (parse_movies())) in
    print_string ("The average gross is: " ^ average ^ "\n")
;;

let do_sort_by_gross  () : unit = wrap_movies sort_by_gross;;
let do_sort_by_year   () : unit = wrap_movies sort_by_year;;
let do_sort_by_studio () : unit = wrap_studios sort_by_studio;;

let do_by_studio () : unit = print_studios (by_studio (parse_movies ()));;

(***************)
(* Main Driver *)
(***************)

let options = [
  ("-average",    Arg.Unit do_average,       "average gross");
  ("-by-studio",  Arg.Unit do_by_studio,     "total gross for each studio");
  ("-decade",     Arg.Int  do_decade,        "select movies from given decade:"
                                             ^ " 20, 30, ..., 90, 00, 10");
  ("-drop",       Arg.Int  do_drop,          "drop first n movies");
  ("-echo",       Arg.Unit do_echo,          "check file for format errors");
  ("-sort-gross", Arg.Unit do_sort_by_gross, "sort by gross");
  ("-sort-year",  Arg.Unit do_sort_by_year,  "sort by year");
  ("-sort-studio",Arg.Unit do_sort_by_studio,"sort studio format by gross");
  ("-take",       Arg.Int  do_take,          "take first n movies");
];;

let main () =
  let usage_message = "Usage: boxoffice option" in
  let anonymous comm = prerr_string ("unknown option " ^ comm ^ "\n") in
    Arg.parse options anonymous usage_message
;;

main ();;
