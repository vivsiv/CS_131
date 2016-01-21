type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

let rec get_rh_side_list rules nt_symbol = 
	match rules with
		| [] -> []
		| (sym,rh_side)::rules_tail -> 
			if sym = nt_symbol
			then rh_side::(get_rh_side_list rules_tail nt_symbol)
			else get_rh_side_list rules_tail nt_symbol
;;

let rules_function rules nt_symbol  =
	get_rh_side_list rules nt_symbol
;;

let convert_grammar gram1 = 
	match gram1 with
		| (start_sym,rules) -> (start_sym, rules_function rules)
;;

let get_rules_function gram2 = 
	match gram2 with
		| (start_sym,rules_fn) -> rules_fn
;;

(*
let parse_prefix gram = 

;; *)