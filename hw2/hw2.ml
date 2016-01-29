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

let rec check_rule rule rules_fn accept derivation frag = 
	match rule with
		| [] -> accept derivation frag
		| symbol::rule_tail -> 
			match frag with
				| [] -> None
				| frag_head::frag_tail -> 
					match symbol with
						| N non_terminal -> 
							let nt_rules_list = rules_fn non_terminal in
							let new_accept = check_rule (rule_tail) (rules_fn) (accept) in
							check_rh_sides (non_terminal) (nt_rules_list) (rules_fn) (new_accept) (derivation) (frag)
						| T terminal ->
							if terminal = frag_head
							then check_rule (rule_tail) (rules_fn) (accept) (derivation) (frag_tail)
							else None

and check_rh_sides start_sym rules_list rules_fn accept derivation frag = 
	match rules_list with
		| [] -> None
		| rule::rules_tail -> 
			let added_derivation = derivation @ [(start_sym,rule)] in
			let match_found = check_rule (rule) (rules_fn) (accept) (added_derivation) (frag) in
			match match_found with
				| None -> check_rh_sides (start_sym) (rules_tail) (rules_fn) (accept) (derivation) (frag)
				| match_value -> match_value
;;

let parse_prefix gram accept frag = 
	match gram with
		| (start_sym, rules_fn) -> 
			let rules_list = rules_fn start_sym in
			check_rh_sides (start_sym) (rules_list) (rules_fn) (accept) ([]) (frag)
;;