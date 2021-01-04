let list1 = [3;2;4;4;5;7;8;9];;
let list2 = [3;5;6;7;7;7;10;11];;

(* Test cases Problem 1 *)
let my_subset_test0 = subset [] list1;;
let my_subset_test1 = subset list1 list2 = false;;
let my_subset_test2 = subset [] [];;
let my_subset_test3 = subset list1 [] = false;;

(* Test cases Problem 2 *)
let my_equal_sets_test0 = equal_sets [] [];;
let my_equal_sets_test1 = equal_sets [1;1;1;2;3] [1;2;2;3];;
let my_equal_sets_test2 = not (equal_sets list1 list2);;

(* Test cases Problem 3 *)
let my_set_union_test0 = equal_sets (set_union [1;2;3] [4;5;6]) [1;2;3;4;5;6];;
let my_set_union_test1 = equal_sets (set_union [] []) [];;
let my_set_union_test2 = equal_sets (set_union [] [1;1;1;2;3]) [1;2;3];;

(* Test cases Problem 4 *)
let my_set_intersection_test0 = equal_sets (set_intersection [1;2;3] [1;2]) [1;2];;
let my_set_intersection_test1 = equal_sets (set_intersection [] [1;2]) [];;
let my_set_intersection_test2 = equal_sets (set_intersection [1;2;3;4;5;5] [1;2;5]) [5;1;2];;
let my_set_intersection_test3 = equal_sets (set_intersection [1;2;3] []) [];;

(* Test cases Problem 5 *)
let my_set_diff_test0 = equal_sets (set_diff [1;2;3] [2;3]) [1];;
let my_set_diff_test1 = equal_sets (set_diff [] [2;3]) [];;
let my_set_diff_test2 = equal_sets (set_diff [1;2;3] [5;2;3]) [1];;

(* Test cases Problem 6 *)
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x/2) 1000000 = 0;; 
let my_computed_fixed_point_test1 = computed_fixed_point (>) (fun x -> -2*x - 3) 0 = -3;; 

type my_nonterminals = 
    | Hello | What | Did | Two | Bye

let my_rules =
  [Hello, [T"["; N Did; T"H"];
  Hello, [N Two];
  Two, [N What; T"OK"];
  Did, [N Bye;T"jk"; N What];
  Bye, [N What; T"bc"];
  What, [T"0"];
  What, [T"1"];
  What, [T"2"]];;

let my_test1_results = 
  [Hello, [N Two];
  Two, [N What; T"OK"];
  What, [T"0"];
  What, [T"1"];
  What, [T"2"]];;

let my_grammar = Hello, my_rules;;

let my_grammar_test1 = Hello, List.tl my_rules;;
let my_grammar_test1_result = Hello, my_test1_results;;


(* Test cases Problem 7 *)
let my_filter_reachable_test0 = filter_reachable my_grammar = my_grammar;;
let my_filter_reachable_test1 = filter_reachable my_grammar_test1 = my_grammar_test1_result;;

