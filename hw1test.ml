Printf.printf "--- Begin My Tests ----\n";;

let my_subset_test0 = subset [1;2] [1;2;3;4];;
let my_subset_test1 = not (subset [1;2;3;4] [1;2]);;
let my_subset_test2 = not (subset [1;2] [5;6;7]);;

let my_equal_sets_test0 = equal_sets [1;2] [1;2];;
let my_equal_sets_test1 = not (equal_sets [1;2] [1;2;3;4]);;

let my_union_test0 = equal_sets [1;2;5;6;7] (set_union [1;2] [5;6;7]);;
let my_union_test1 = equal_sets [1;2;3;4] (set_union [1;2] [1;2;3;4]);;

let my_intersect_test0 = equal_sets [] (set_intersection [1;2] [5;6;7]);;
let my_intersect_test1 = equal_sets [1;2] (set_intersection [1;2] [1;2;3;4]);;

let my_set_diff_test0 = equal_sets [] (set_diff [1;2] [1;2;3;4]);;
let my_set_diff_test1 = equal_sets [3;4] (set_diff [1;2;3;4] [1;2]);;

let my_computed_fixed_point_test0 = (computed_fixed_point (=) (fun x -> x *. (1. /. x)) (5.)) = 1.;;
let my_computed_fixed_point_test1 = (computed_fixed_point (=) (fun x -> x *. 2.) (1.)) = infinity;;

let my_computed_periodic_point_test0 = (computed_periodic_point (=) (fun x -> -x) (2) (5)) = 5;;

type all_alleys_nonterminals = | A | B | C | D | E;;

let all_alleys_rules =
   [A, [N B];
    B, [N C; N D; N A];
    C, [N D];
    D, [N E; N B];
    E, [N A; N C]]
;;

let all_alleys_grammar = A, all_alleys_rules;;

let my_filter_blind_alleys_test0 = (filter_blind_alleys all_alleys_grammar = (A,[]));;

let some_alleys_rules = 
   [A, [N B];
    B, [N C; N D; N A];
    B, [T "Yo"];
    C, [N D];
    D, [N E; N B];
    E, [N A; N C]]
;;

let some_alleys_grammar = A, some_alleys_rules;;

let my_filter_blind_alleys_test1 = 
  (filter_blind_alleys some_alleys_grammar = 
    (A,  
      [A, [N B];
       B, [T "Yo"]]
    ))
;;