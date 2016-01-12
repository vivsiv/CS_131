let debug = true;;

let rec print_int_list l = 
	match l with 
		| [] -> print_string "\n";
		| h::t -> print_int h; print_string " "; print_int_list t
;;

let rec contains l a =
	match l with
		| [] -> false
		| h::t -> if a == h then true else contains t a
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
let rules3 = [Lvalue, [N Lvalue]; Binop, [N Expr]; Expr, [T "("; N Expr; T ")"]; Expr, [T "5"]; Incrop, [T "++"]];;

let rec right_side_terminates rh_side terminal_symbols = 
	match rh_side with
		| [] -> true
		| symbol::rh_side_tail ->
			match symbol with 
				| N non_terminal -> 
					if contains terminal_symbols non_terminal
					then right_side_terminates rh_side_tail terminal_symbols
					else false
				| T terminal -> right_side_terminates rh_side_tail terminal_symbols
;;

let rec add_terminating_symbols (all_rules, scanned_rules, base_terminal_symbols) =
	match scanned_rules with
		| [] -> (all_rules, all_rules, base_terminal_symbols)
		| (nt_symbol,rh_side)::scanned_rules_tail -> 
			if right_side_terminates rh_side base_terminal_symbols 
			then 
				if subset [nt_symbol] base_terminal_symbols
				then add_terminating_symbols (all_rules, scanned_rules_tail, base_terminal_symbols)
				else add_terminating_symbols (all_rules, scanned_rules_tail, (nt_symbol::base_terminal_symbols))
			else
				add_terminating_symbols (all_rules, scanned_rules_tail, base_terminal_symbols)
;;

let terminal_symbol_list (all_rules, scanned_rules, terminal_symbols) =
	terminal_symbols
;;

let equal_symbol_lists s1 s2 = 
	equal_sets (terminal_symbol_list s1) (terminal_symbol_list s2)
;;

if debug then Printf.printf "add_terminating_symbols rules: %B\n" (terminal_symbol_list(add_terminating_symbols(rules,rules,[])) = [Incrop;Expr]);;
if debug then Printf.printf "add_terminating_symbols rules2: %B\n" (terminal_symbol_list(add_terminating_symbols(rules2,rules2,[])) = [Expr;Binop]);;
if debug then Printf.printf "add_terminating_symbols rules3: %B\n" (terminal_symbol_list(add_terminating_symbols(rules3,rules3,[])) = [Incrop;Expr]);;

if debug then Printf.printf "add_terminating_symbols 1x rules: %B\n" (terminal_symbol_list(add_terminating_symbols(rules,rules,[])) = [Incrop;Expr]);;
if debug then Printf.printf "add_terminating_symbols 2x rules: %B\n" (terminal_symbol_list(add_terminating_symbols(add_terminating_symbols(rules,rules,[]))) = [Binop;Incrop;Expr]);;
if debug then Printf.printf "add_terminating_symbols 3x rules: %B\n" (terminal_symbol_list(add_terminating_symbols(add_terminating_symbols(add_terminating_symbols(rules,rules,[])))) = [Lvalue;Binop;Incrop;Expr]);;
if debug then Printf.printf "add_terminating_symbols 4x rules: %B\n" (terminal_symbol_list(add_terminating_symbols(add_terminating_symbols(add_terminating_symbols(add_terminating_symbols(rules,rules,[]))))) = [Lvalue;Binop;Incrop;Expr]);;

if debug then Printf.printf "add_terminating_symbols 1x rules2: %B\n" (terminal_symbol_list(add_terminating_symbols(rules2,rules2,[])) = [Expr;Binop]);;
if debug then Printf.printf "add_terminating_symbols 2x rules2: %B\n" (terminal_symbol_list(add_terminating_symbols(add_terminating_symbols(rules2,rules2,[]))) = [Expr;Binop]);;

if debug then Printf.printf "add_terminating_symbols 1x rules3: %B\n" (terminal_symbol_list(add_terminating_symbols(rules3,rules3,[])) = [Incrop;Expr]);;
if debug then Printf.printf "add_terminating_symbols 2x rules3: %B\n" (terminal_symbol_list(add_terminating_symbols(add_terminating_symbols(rules3,rules3,[]))) = [Binop;Incrop;Expr]);;
if debug then Printf.printf "add_terminating_symbols 3x rules3: %B\n" (terminal_symbol_list(add_terminating_symbols(add_terminating_symbols(add_terminating_symbols(rules3,rules3,[])))) = [Binop;Incrop;Expr]);;

let find_all_terminating_symbols rules =  
	terminal_symbol_list(computed_fixed_point (equal_symbol_lists) add_terminating_symbols (rules,rules,[]))
;; 

if debug then Printf.printf "find_all_terminating_symbols rules: %B\n" (find_all_terminating_symbols(rules) = [Lvalue;Binop;Incrop;Expr]);;
if debug then Printf.printf "find_all_terminating_symbols rules2: %B\n" (find_all_terminating_symbols(rules2) = [Expr;Binop]);;
if debug then Printf.printf "find_all_terminating_symbols rules3: %B\n" (find_all_terminating_symbols(rules3) = [Binop;Incrop;Expr]);;

let rec filter_rules rules terminal_symbols = 
	match rules with
		| [] -> []
		| (nt_symbol,rh_side)::rules_tail -> 
			if right_side_terminates rh_side terminal_symbols
			then (nt_symbol,rh_side)::(filter_rules rules_tail terminal_symbols)
			else filter_rules rules_tail terminal_symbols
;;	

let filter_blind_alleys g =
	match g with
		| (start_sym,rules) -> (start_sym, filter_rules rules (find_all_terminating_symbols rules))
;;

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num
;;

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]
;;

let awksub_grammar = Expr, awksub_rules;;

let awksub_test0 =
  filter_blind_alleys awksub_grammar = awksub_grammar;;

let awksub_test1 =
  filter_blind_alleys (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules);;

let awksub_test2 =
  filter_blind_alleys (Expr,
      [Expr, [N Num];
       Expr, [N Lvalue];
       Expr, [N Expr; N Lvalue];
       Expr, [N Lvalue; N Expr];
       Expr, [N Expr; N Binop; N Expr];
       Lvalue, [N Lvalue; N Expr];
       Lvalue, [N Expr; N Lvalue];
       Lvalue, [N Incrop; N Lvalue];
       Lvalue, [N Lvalue; N Incrop];
       Incrop, [T"++"]; Incrop, [T"--"];
       Binop, [T"+"]; Binop, [T"-"];
       Num, [T"0"]; Num, [T"1"]; Num, [T"2"]; Num, [T"3"]; Num, [T"4"];
       Num, [T"5"]; Num, [T"6"]; Num, [T"7"]; Num, [T"8"]; Num, [T"9"]])
  = (Expr,
     [Expr, [N Num];
      Expr, [N Expr; N Binop; N Expr];
      Incrop, [T"++"]; Incrop, [T"--"];
      Binop, [T "+"]; Binop, [T "-"];
      Num, [T "0"]; Num, [T "1"]; Num, [T "2"]; Num, [T "3"]; Num, [T "4"];
      Num, [T "5"]; Num, [T "6"]; Num, [T "7"]; Num, [T "8"]; Num, [T "9"]])
;;

let awksub_test3 =
  filter_blind_alleys (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    filter_blind_alleys (Expr, List.tl (List.tl awksub_rules))
;;


if debug then Printf.printf "awksub_test0: %B\n" awksub_test0;;
if debug then Printf.printf "awksub_test1: %B\n" awksub_test1;;
if debug then Printf.printf "awksub_test2: %B\n" awksub_test2;;
if debug then Printf.printf "awksub_test3: %B\n" awksub_test3;;





