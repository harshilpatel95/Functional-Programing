(* Definitions for sets. *)

exception TODO

(* An interface for set modules *)
module type SET = 
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it 
   * and returns that element plus the new set.  
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. See "Testing" in the project description web page *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some 
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE = 
sig
  type t
  val compare : t -> t -> Order.order
  val string_of_t : t -> string

  (* The functions below are used for testing.
     See "Testing" in the project description web page *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () = 
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) = 
struct
  open Order
  type elt = C.t 
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs = 
    match xs with 
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs = 
    match xs with 
      | [] -> [x]
      | y::ys -> (match C.compare x y with 
          | Greater -> y::(insert x ys)
          | Eq -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right insert xs ys
  let rec remove y xs = 
    match xs with 
      | [] -> []
      | x::xs1 -> (match C.compare y x with 
          | Eq -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys = 
    match xs, ys with 
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with 
          | Eq -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x = 
    match xs with 
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Eq -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs = 
    match xs with 
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left (fun a x -> f x a) e 
    
  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string = 
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"


  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set = 
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
    ()

  let test_fold () =
    ()

  let test_is_empty () =
    ()

  let test_singleton () =
    ()

  let run_tests () = 
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) = 
struct
  module D = Dict.Make(struct
    type key = C.t 
    type value = unit 
    let compare = C.compare
    let string_of_key = C.string_of_t
    let string_of_value () = "()"

    let gen_key  = C.gen
    let gen_key_random  = C.gen_random
    let gen_key_gt  = C.gen_gt
    let gen_key_lt  = C.gen_lt
    let gen_key_between  = C.gen_between
    let gen_value () = ()
    let gen_pair () = (gen_key_random(), gen_value())
end)

  open Order
  type elt = D.key
  type set = D.dict
  let empty = D.empty

  let is_empty s = (D.choose s = None)
  let insert k s = D.insert s k ()
  let singleton k = D.insert empty k ()
  let remove k s = D.remove s k
  let member = D.member
  let choose s =
    match D.choose s with
    | Some(k,v,d) -> Some(k,d)
    | None -> None
		
  let fold f x s = D.fold (fun k v x -> f k x) x s 
  let union s1 s2 = fold insert s1 s2
  let rec intersect s1 s2 =
    match D.choose s1, D.choose s2 with
    | None, _ -> empty
    | _, None -> empty
    | Some(s1_hd, _,  s1'), Some(s2_hd, _, s2') ->
       (match C.compare s1_hd s2_hd with
	| Eq -> D.insert (intersect s1' s2') s1_hd ()
	| Less -> intersect s1' s2
	| Greater -> intersect s1 s2')

  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

  (* add your test functions to run_tests *)
  let insert_list (d: set) (lst: elt list) : set = 
    List.fold_left (fun r k -> insert k r) d lst
		  
  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()
      
  let test_choose () =
    assert (choose empty = None);

    let elt = C.gen_random () in
    assert(choose (singleton elt) = Some(elt, empty));

    (*note reverse order for not BT dicts*)
    
    let elt = C.gen_random() in
    let elt_gt = C.gen_gt elt () in
    let d = insert elt_gt (singleton elt) in
    assert (choose d = Some(elt, singleton elt_gt));

    let elt = C.gen_random() in
    let elt_gt = C.gen_gt elt () in
    let elt_lt = C.gen_lt elt () in
    let d = insert elt (insert elt_lt (singleton elt_gt)) in
    (match choose d with
     | Some(e1, d1) ->
	(assert(e1 = elt_lt);
	 (match choose d1 with
	  | Some(e2, d2) ->
	     (assert (e2 = elt);
	      (match choose d2 with
	       | Some (e3, d3) ->
		  assert((e3, d3) = (elt_gt, empty))
	       | None -> assert(not true) (*fail*) ))
	  | None  -> assert(not true) (*fail*) ))
     | None -> assert(not true) (*fail*));

    ()

  let test_fold () =
    let print_func = (fun elt s -> s ^ (C.string_of_t elt)) in
    let elt = C.gen_random () in
    let elt_lt = C.gen_lt elt () in
    let elt_gt = C.gen_gt elt () in
    let d = insert elt (insert elt_lt (singleton elt_gt)) in
    (assert(fold print_func "" empty = "");
     assert(fold print_func "" d =
	   (C.string_of_t elt_lt) ^ (C.string_of_t elt) ^ (C.string_of_t elt_gt)));
    ()
      
  let test_is_empty () =
    assert(is_empty empty = true);
    assert(is_empty (singleton (C.gen_random())) = false);
    assert(is_empty (insert (C.gen_random()) (singleton (C.gen_random()))) = false);
    ()
      
  let test_singleton () =
    let elt = C.gen_random() in
    assert(singleton elt = insert elt empty);
    ()
      
  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ; ()

  let test_union () = 
    let elts1 = generate_random_list 50 in
    let s1 = insert_list empty elts1 in
    let elts2 = generate_random_list 50 in
    let s2 = insert_list empty elts2 in
    let s = union s1 s2 in (
    List.iter(fun k-> assert(member s k)) elts1;
    List.iter(fun k-> assert(member s k)) elts2 
    );  ()

  let test_intersect () =
    let elt = C.gen_random () in
    let elt_gt = C.gen_gt elt () in
    let elt_lt = C.gen_lt elt () in
    let d = singleton elt in
    let d_lt = insert elt_lt d in 
    let d_gt = insert elt_gt d in
    assert(intersect d_lt d_gt = d);

    let elts1 = generate_random_list 50 in
    let s1 = insert_list empty elts1 in
    let elts2 = generate_random_list 50 in
    let s2 = insert_list empty elts2 in
    let s = intersect s1 s2 in
    List.iter(fun k->
	      assert((member s1 k && member s2 k) = member s k)) (elts1@elts2)
      ; ()
    
  let test_member () =
    assert(member empty (C.gen_random ()) = false);

    let elt = C.gen_random () in
    let elt_lt = C.gen_lt elt () in
    let d = singleton elt in
    (assert(member d elt = true);
     assert(member d elt_lt = false));

    let elt = C.gen_random () in
    let elt_lt = C.gen_lt elt () in
    let elt_gt = C.gen_gt elt () in
    let d = insert elt_lt (insert elt empty) in
    (assert(member d elt = true);
     assert(member d elt_lt = true);
     assert(member d elt_gt = false));
    ()
    
      
  let run_tests () = 
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member (); 
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ; ()

end




(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
(*module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests();;*)

(* Create a set of ints using our DictSet functor
 * 
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)

module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;



(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) = 
  (* Change this line to use our dictionary implementation when your are 
   * finished. *)
  (*ListSet (C)*)
   DictSet (C) 

