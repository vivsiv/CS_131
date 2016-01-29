let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None
   
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

type english_nonterminals =
  | Sentence | NounPhrase | VerbPhrase | Noun | Verb | Article | Adjective | Adverb
;;

let english_grammar = (Sentence,
  function | Sentence -> [[N NounPhrase; N VerbPhrase]]
    | NounPhrase -> [[N Noun]; [N Adjective; N Noun]; [N Article; N Noun]; [N Article; N NounPhrase]]
    | VerbPhrase -> [[N Verb]; [N Verb; N NounPhrase]; [N Verb; N Adverb]]
    | Noun -> [[T"warriors"]; [T"playing"]; [T"oracle"]]
    | Verb -> [[T"are"]; [T "remain"]]
    | Article -> [[T"the"]]
    | Adjective -> [[T "1st place"];[T"2nd place"];[T"skilled"]]
    | Adverb -> [[T "lowly"]; [T "highly"; N Adjective]])

let test_0 = ((parse_prefix english_grammar accept_all ["the"; "warriors"; "are"; "playing"])
  = Some ([(Sentence, [N NounPhrase; N VerbPhrase]); 
	   (NounPhrase, [N Article; N Noun]); 
	   (Article, [T "the"]);
	   (Noun, [T "warriors"]);
	   (VerbPhrase, [N Verb]); 
	   (Verb, [T "are"])], ["playing"]))
;;

let test_1 = ((parse_prefix english_grammar accept_empty_suffix ["the"; "warriors"; "are"; "playing"])
  = Some ([(Sentence, [N NounPhrase; N VerbPhrase]); 
	   (NounPhrase, [N Article; N Noun]); 
	   (Article, [T "the"]);
	   (Noun, [T "warriors"]);
	   (VerbPhrase, [N Verb; N NounPhrase]); 
	   (Verb, [T "are"]);
	   (NounPhrase, [N Noun]); 
	   (Noun, [T "playing"])], []))
;;

let test_2 = ((parse_prefix english_grammar accept_empty_suffix ["the"; "1st place"; "warriors"; "remain"; "highly"; "skilled"])
  = Some ([(Sentence, [N NounPhrase; N VerbPhrase]); 
	   (NounPhrase, [N Article; N NounPhrase]); 
	   (Article, [T "the"]);
	   (NounPhrase, [N Adjective; N Noun]);
	   (Adjective, [T "1st place"]);
	   (Noun, [T "warriors"]);
	   (VerbPhrase, [N Verb; N Adverb]); 
	   (Verb, [T "remain"]); 
	   (Adverb, [T "highly"; N Adjective]);
	   (Adjective, [T "skilled"])], []))
;;

let test_3 = ((parse_prefix english_grammar accept_all ["the"; "1st place"; "warriors"; "remain"; "highly"; "skilled"])
  = Some ([(Sentence, [N NounPhrase; N VerbPhrase]); 
	   (NounPhrase, [N Article; N NounPhrase]); 
	   (Article, [T "the"]);
	   (NounPhrase, [N Adjective; N Noun]);
	   (Adjective, [T "1st place"]);
	   (Noun, [T "warriors"]);
	   (VerbPhrase, [N Verb]); 
	   (Verb, [T "remain"])], ["highly"; "skilled"]))
;;

