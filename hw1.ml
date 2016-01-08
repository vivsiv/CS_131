let debug = true;;

let rec print_int_list l = 
	match l with 
		| [] -> print_string "\n";
		| h::t -> print_int h; print_string " "; print_int_list t
;;

let rec contains l a =
	match l with
		| [] -> false
		| h::t -> if a = h then true else contains t a
;;
let l = [1;2;3];;
if debug then Printf.printf "[1;2;3] contains 1? : %B\n" (contains l 1);;
if debug then Printf.printf "[1;2;3] contains 4? : %B\n" (contains l 4);;

let rec subset a b = 
	match a with
		| [] -> true
		| h::t -> if contains b h then subset t b else false
;;

let a = [1;2];;
let b = [1;2;3;4];;
let c = [5;6;7];;
if debug then Printf.printf "[1;2] subset [1;2;3;4]? : %B\n" (subset a b);;
if debug then Printf.printf "[1;2;3;4] subset [1;2]? : %B\n" (subset b a);;
if debug then Printf.printf "[1;2] subset [5;6;7]? : %B\n" (subset a c);;

let equal_sets a b = 
	(subset a b) && (subset b a)
;;

let d = [1;2];;
if debug then Printf.printf "[1;2] equal [1;2]? : %B\n" (equal_sets a d);;
if debug then Printf.printf "[1;2] equal [1;2;3;4]? : %B\n" (equal_sets a b);;

let rec set_union a b =
	match a with
		| [] -> b
		| h::t -> if contains b h then set_union t b else set_union t (h::b) 
;;
if debug then Printf.printf "[1;2] union [5;6;7]? : "; print_int_list (set_union a c);;
if debug then Printf.printf "[1;2] union [1;2;3;4]? : "; print_int_list (set_union a b);;

let rec intersect a b base =
	match a with 
		| [] -> base
		| h::t -> if contains b h then intersect t b (h::base) else intersect t b base
;;

let set_intersection a b = intersect a b [];;

if debug then Printf.printf "[1;2] intersect [5;6;7]? : "; print_int_list (set_intersection a c);;
if debug then Printf.printf "[1;2] intersect [1;2;3;4]? : "; print_int_list (set_intersection a b);;
(*if debug then Printf.printf "[1;2] subset [5;6;7]? : %B\n" (subset a c);;*)

let rec difference a intersect base = 
	match a with
		| [] -> base
		| h::t -> if contains intersect h then difference t intersect base else difference t intersect (h::base)
;;
let rec set_difference a b = 
	difference a (set_intersection a b) []
;;
if debug then Printf.printf "[1;2] difference [1;2;3;4]? : "; print_int_list (set_difference a b);;
if debug then Printf.printf "[1;2;3;4] difference [1;2]? : "; print_int_list (set_difference b a);;

let rec fixed_point eq f x num_calls =
	if eq (f x) x then num_calls + 1 else fixed_point eq f (f x) (num_calls + 1)
;;

let rec computed_fixed_point eq f x =
	fixed_point eq f x 0
;;
