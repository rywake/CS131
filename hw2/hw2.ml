type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
| Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
| Leaf of 'terminal

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num
  

(* Ask why can't do match statement for second part *)
let rec concat_symbol_rules rules sym = match rules with
  |[] -> []
  |hd::tl -> if sym = fst hd then 
              (snd hd)::concat_symbol_rules tl sym 
              else concat_symbol_rules tl sym;;

let convert_grammar gram1 = (fst gram1, concat_symbol_rules (snd gram1));;


(* Helper function for parse tree *)
let rec parse_tree_list treeList = match treeList with
  |[] -> []
  |hd::tl -> match hd with
            |Leaf x -> x::parse_tree_list tl
            |Node (newNode, newList) -> parse_tree_list newList@(parse_tree_list tl);;

let parse_tree_leaves tree = match tree with
  |Node (_, treeList) -> parse_tree_list treeList
  |_ -> [];;
  

(* This is for a specific rule i.e. ([N Expr; T"("]) *)

let rec process_grammar myGrammarFunc current_rule acceptor fragList = match current_rule with
  |[] -> acceptor fragList
  |hd::tl -> match hd with
      |N nonterminal -> let process_newnonT = process_grammar myGrammarFunc tl acceptor in (* Function to help with backtracking if current path doesn't match the fragment. Will allow all paths to be checked. *)
                        process_nonterminal_rules myGrammarFunc (myGrammarFunc nonterminal) process_newnonT fragList
      |T terminal -> match fragList with
            |[] -> None
            | fhd::ftl ->  if fhd = terminal then process_grammar myGrammarFunc tl acceptor ftl else None
(* This is for a list of rules of nonterminals i.e. (Expr -> [[N Expr]; [N Expr; N Binop; T")"]...]) This is a list of lists of type symbol *)
and process_nonterminal_rules myGrammarFunc grammar_rulesList acceptor fragList = match grammar_rulesList with
  |[] -> None
  |hd::tl -> let resulting_grammar = process_grammar myGrammarFunc hd acceptor fragList in
              if resulting_grammar = None
              then process_nonterminal_rules myGrammarFunc tl acceptor fragList
              else resulting_grammar;;

(* Main function to implement make_matcher*)
let make_matcher gram1 =  
  let (start, rulesFunc) = gram1 in
  let starting_rules = rulesFunc start in
  process_nonterminal_rules rulesFunc starting_rules;;



(* PROBLEM 4 Start of code to implement make_parser *)

(* Acceptor to create the path *)
let parse_acceptor path_list = function
|[] -> Some path_list
| _ -> None;;


let rec get_grammar_path prev_nonTsymbol myGrammarFunc current_rule acceptor path fragList = match current_rule with
  |[] -> acceptor path fragList
  |hd::tl -> match hd with
      |N nonterminal -> let process_newnonT = get_grammar_path prev_nonTsymbol myGrammarFunc tl acceptor in (* Function to help with backtracking if current path doesn't match the fragment. Will allow all paths to be checked. *)
                    process_nonterminal_path nonterminal myGrammarFunc (myGrammarFunc nonterminal) process_newnonT path fragList
      |T terminal -> match fragList with
            |[] -> None
            | fhd::ftl ->  if fhd = terminal then 
              get_grammar_path prev_nonTsymbol myGrammarFunc tl acceptor path ftl
              else None
(* This is for a list of rules of nonterminals i.e. (Expr -> [[N Expr]; [N Expr; N Binop; T")"]...]) This is a list of lists of type symbol *)
and process_nonterminal_path prev_nonTsymbol myGrammarFunc grammar_rulesList acceptor path fragList = match grammar_rulesList with
  |[] -> None
  |hd::tl -> let resulting_grammar = get_grammar_path prev_nonTsymbol myGrammarFunc hd acceptor ((prev_nonTsymbol, hd)::path) fragList in
              if resulting_grammar = None
              then process_nonterminal_path prev_nonTsymbol myGrammarFunc tl acceptor path fragList
              else resulting_grammar;;

(* Updates the path to reflect used parse_list object *)
let rec new_tuples old_path total_path = match old_path with
  |[] -> total_path
  |hd::tl -> new_tuples tl (List.tl total_path);;

let rec get_tuples_seen current_rule tuples_list = match current_rule with
  | [] -> []
  | hd::tl -> match hd with
              | T terminal -> (get_tuples_seen tl tuples_list)
              | N nonterminal -> let newList = (get_tuples_seen tl tuples_list) in
                                  newList@(create_graph_tuple (new_tuples newList tuples_list))
and create_graph_tuple tuples_list = match tuples_list with
  | [] -> []
  | hd::tl -> let (curr_node, grammar_rule) = hd in
              hd::(get_tuples_seen grammar_rule tl);;


let rec create_graph_list current_rule tuples_list = match current_rule with
  | [] -> []
  | hd::tl -> match hd with
              | T terminal -> (Leaf terminal)::(create_graph_list tl tuples_list)
              | N nonterminal -> let new_path = (create_graph_node tuples_list) in
                                  let seen_path = create_graph_tuple tuples_list in
                                  let get_tuples = new_tuples seen_path tuples_list in
                                  new_path@(create_graph_list tl get_tuples)
and create_graph_node tuples_list = match tuples_list with
  | [] -> []
  | hd::tl -> let (curr_node, grammar_rule) = hd in
              [Node(curr_node, create_graph_list grammar_rule tl)];;


let remove_some myList = match myList with
|Some x -> x
|_ -> [];;


let make_parse_helper gram fragment = 
  let (start, rulesFunc) = gram in
  let starting_rules = rulesFunc start in
  let graph = process_nonterminal_path start rulesFunc starting_rules parse_acceptor [] fragment in
  match graph with
  |None -> None
  |Some x -> let new_graph = List.rev x in
              let final = create_graph_node new_graph in
              Some (List.hd final);;

let make_parser gram = 
  make_parse_helper gram;;

