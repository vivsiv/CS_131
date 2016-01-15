let rec contains l a =
	match l with
		| [] -> false
		| h::t -> if a == h then true else contains t a
;;

let rec subset a b = 
	match a with
		| [] -> true
		| h::t -> if contains b h then subset t b else false
;;

let equal_sets a b = 
	(subset a b) && (subset b a)
;;

let rec set_union a b =
	match a with
		| [] -> b
		| h::t -> if contains b h then set_union t b else set_union t (h::b) 
;;

let rec intersect a b base =
	match a with 
		| [] -> base
		| h::t -> if contains b h then intersect t b (h::base) else intersect t b base
;;

let set_intersection a b = intersect a b [];;

let rec difference a intersect base = 
	match a with
		| [] -> base
		| h::t -> if contains intersect h then difference t intersect base else difference t intersect (h::base)
;;
let rec set_diff a b = 
	difference a (set_intersection a b) []
;;

let rec computed_fixed_point eq f x =
	if eq (f x) x 
	then x 
	else computed_fixed_point eq f (f x)
;;

let rec run_period f p x = 
	if p = 0 then x else run_period f (p - 1) (f x)
;;

let rec computed_periodic_point eq f p x =
	let period_x = run_period f p x in
	if eq period_x x then x else computed_periodic_point eq f p (f x)
;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

let rec rh_side_terminates rh_side terminal_symbols = 
	match rh_side with
		| [] -> true
		| symbol::rh_side_tail ->
			match symbol with 
				| N non_terminal -> 
					if contains terminal_symbols non_terminal
					then rh_side_terminates rh_side_tail terminal_symbols
					else false
				| T terminal -> rh_side_terminates rh_side_tail terminal_symbols
;;

let rec add_terminating_symbols (all_rules, scanned_rules, base_terminal_symbols) =
	match scanned_rules with
		| [] -> (all_rules, all_rules, base_terminal_symbols)
		| (nt_symbol,rh_side)::scanned_rules_tail -> 
			if rh_side_terminates rh_side base_terminal_symbols 
			then 
				if contains base_terminal_symbols nt_symbol
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

let find_all_terminating_symbols rules =  
	terminal_symbol_list(computed_fixed_point (equal_symbol_lists) add_terminating_symbols (rules,rules,[]))
;;

let rec filter_rules rules terminal_symbols = 
	match rules with
		| [] -> []
		| (nt_symbol,rh_side)::rules_tail -> 
			if rh_side_terminates rh_side terminal_symbols
			then (nt_symbol,rh_side)::(filter_rules rules_tail terminal_symbols)
			else filter_rules rules_tail terminal_symbols
;;	

let filter_blind_alleys g =
	match g with
		| (start_sym,rules) -> (start_sym, filter_rules rules (find_all_terminating_symbols rules))
;;





