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

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

type nonterms = | Expr | Binop | Lvalue | Incrop;;

let rule = Expr, [T "("; N Expr; T ")"];;
let rules = [Lvalue, [N Binop]; Binop, [N Expr]; Expr, [T "("; N Expr; T ")"]; Expr, [T "5"]; Incrop, [T "++"]];;
let rules2 = [Binop, [T "+"]; Expr, [T "5"]];;

let symbol_terminates sym term_symbols = 
	match sym with 
		| N s -> subset [s] term_symbols
 		| T s -> true
;;

let rec right_side_terminates rh_side term_symbols = 
	match rh_side with
		| [] -> true
		| h::t -> if (symbol_terminates h term_symbols) then right_side_terminates t term_symbols else false
;;


let rec find_terminating_symbols term_symbols rules =
	match rules with
		| [] -> term_symbols
		| (sym,l)::t -> 
			if (right_side_terminates l term_symbols) 
			then 
				if subset [sym] term_symbols
				then find_terminating_symbols term_symbols t
				else find_terminating_symbols (sym::term_symbols) t
			else
				find_terminating_symbols term_symbols t
;;

if debug then Printf.printf "find_terminating_symbols rules: %B\n" ((find_terminating_symbols [] rules) = [Incrop;Expr]);;
if debug then Printf.printf "find_terminating_symbols rules2: %B\n" ((find_terminating_symbols [] rules2) = [Expr;Binop]);;

let rec rule_terminates r term_symbols =
	match r with
		| (sym,l) -> right_side_terminates l term_symbols

;;

let rec filter_rules rules term_symbols = 
	match rules with
		| [] -> []
		| r::t -> 
			if rule_terminates r term_symbols
			then r::(filter_rules t term_symbols)
			else filter_rules t term_symbols
;;	

let filter_blind_alleys g =
	match g with
		| (start_sym,rules) -> (start_sym, filter_rules rules (find_terminating_symbols [] rules))
;;

let test_grammar = Expr, rules;;
(* let test_filtered_grammar = Expr, rules;; *)
let test_filtered_grammar = Expr, [Binop, [N Expr]; Expr, [T "("; N Expr; T ")"]; Expr, [T "5"]; Incrop, [T "++"]];;

if debug then Printf.printf "filter_blind_alleys rules: %B\n" ((filter_blind_alleys test_grammar) = test_filtered_grammar);;

let test_grammar_2 = Expr, rules2;;
let test_filtered_grammar_2 = Expr, rules2;;
if debug then Printf.printf "filter_blind_alleys rules2: %B\n" ((filter_blind_alleys test_grammar_2) = test_filtered_grammar_2);;








