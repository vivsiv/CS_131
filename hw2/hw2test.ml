type awksub_nonterminals = | Expr | Term | Lvalue | Incrop | Binop | Num;;

let awkish_rules = 
	[Expr, [N Term; N Binop; N Expr];
     Expr, [N Term];
     Term, [N Num];
	 Term, [N Lvalue];
	 Term, [N Incrop; N Lvalue];
	 Term, [N Lvalue; N Incrop];
	 Term, [T"("; N Expr; T")"];
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

let hw1_grammar = Expr, awkish_rules;;

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 	[[N Num];
	  	[N Lvalue];
	  	[N Incrop; N Lvalue];
	  	[N Lvalue; N Incrop];
	  	[T"("; N Expr; T")"]]
     | Lvalue ->
	 	[[T"$"; N Expr]]
     | Incrop ->
	 	[[T"++"];
	  	[T"--"]]
     | Binop ->
	 	[[T"+"];
	  	[T"-"]]
     | Num ->
	 	[[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  	[T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])
;;

let get_rh_side_list_test1 =
	(get_rh_side_list awkish_rules Expr) = [[N Term; N Binop; N Expr];[N Term]]
;;

let get_rh_side_list_test2 =
	(get_rh_side_list awkish_rules Expr) = [[N Num]; [N Lvalue]; [N Incrop; N Lvalue]; [N Lvalue; N Incrop]; [T"("; N Expr; T")"]]
;;

let hw2_grammar = convert_grammar hw1_grammar;;

let convert_grammar_test0 = 
	(get_rules_function hw2_grammar) Expr = (get_rules_function awkish_grammar) Expr
;;

let convert_grammar_test1 = 
	(get_rules_function hw2_grammar) Term = (get_rules_function awkish_grammar) Term
;;

let convert_grammar_test2 = 
	(get_rules_function hw2_grammar) Num = (get_rules_function awkish_grammar) Num
;;

