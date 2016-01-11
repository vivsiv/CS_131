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

let rec computed_fixed_point eq f x =
	if eq (f x) x 
	then x 
	else computed_fixed_point eq f (f x)
;;

if debug then Printf.printf "computed_fixed_point x^2 starting at 0 : %d\n" (computed_fixed_point (=) (fun x -> x * 2) (0));;
if debug then Printf.printf "computed_fixed_point x / 2 starting at 10 : %d\n" (computed_fixed_point (=) (fun x -> x / 2) (10));;
if debug then Printf.printf "computed_fixed_point sqrt(x) starting at 10. : %f\n" (computed_fixed_point (=) (sqrt) (10.));;
if debug then Printf.printf "computed_fixed_point x * 2 starting at 1. : %f\n" (computed_fixed_point (=) (fun x -> x *. 2.) (1.));;

let rec run_period f p x = 
	if p = 0 then x else run_period f (p - 1) (f x)
;;

let rec computed_periodic_point eq f p x =
	let period_x = run_period f p x in
	if eq period_x x then x else computed_periodic_point eq f p (f x)
;;

if debug then Printf.printf "computed_periodic_point x/2 period 0 starting at -1 : %d\n" (computed_periodic_point (=) (fun x -> x /2) (0) (-1));;
if debug then Printf.printf "computed_periodic_point x^2 - 1 period 2 starting at 0.5 : %f\n" (computed_periodic_point (=) (fun x -> x *. x -. 1.) (2) (0.5));;










