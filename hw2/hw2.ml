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

let rec match_rule rule rules_fn frag derivation accept = 
	match frag with
		| [] -> None
		| frag_head::frag_tail -> 
			match rule with
				| [] -> accept derivation frag_tail
				| symbol::rule_tail -> 
					match symbol with
						| N non_terminal -> match_rules (non_terminal) (rules_fn non_terminal) (rules_fn) (frag) (derivation) (accept)
						| T terminal ->
							if terminal = frag_head
							then match_rule (rule_tail) (rules_fn) (frag_tail) (derivation) (accept)
							else None

and match_rules start_sym rules_list rules_fn frag derivation accept = 
	match rules_list with
		| [] -> None
		| rule::rules_tail -> 
			let added_derivation = derivation @ [(start_sym,rule)] in
			let match_found = match_rule (rule) (rules_fn) (frag) (added_derivation) (accept) in
			match match_found with
				| None -> match_rules (start_sym) (rules_tail) (rules_fn) (frag) (derivation) (accept)
				| value -> value
;;

let parse_prefix gram accept frag = 
	match gram with
		| (start_sym, rules_fn) -> match_rules (start_sym) (rules_fn start_sym) (rules_fn) (frag) ([]) (accept)
;;