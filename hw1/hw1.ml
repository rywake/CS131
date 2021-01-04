let subset a b =
        let existsInB element =
                List.mem element b in
        List.for_all existsInB a;;

let equal_sets a b =
        let concSet = a @ b in
        let existsInBoth element =
                let l1 = List.mem element a in
                let l2 = List.mem element b in
                l1 && l2 in
        List.for_all existsInBoth concSet;;

(* Helper Function *)
let rec remove_duplicates myList = match myList with
        | [] -> []
        | hd::tl -> if List.mem hd tl then remove_duplicates tl else hd::remove_duplicates tl;;

let set_union a b =
       let conc = a @ b in
       remove_duplicates conc;;

let set_intersection a b =
        let existsInB element =
                List.mem element b in
        List.filter existsInB a;;
        
let set_diff a b =
        let notInB element =
                if List.mem element b then false else true in
        List.filter notInB a;;

let rec computed_fixed_point eq f x =
        let answer = (f x) in
        if eq answer x then x
        else computed_fixed_point eq f answer;;

type ('nonterminal, 'terminal) symbol =
        | N of 'nonterminal
        | T of 'terminal

(* Helper Function *)
let rec get_Nonterminals symbols = match symbols with
        | [] -> []
        | hd::tl -> match hd with
                | N symbols -> symbols :: get_Nonterminals tl
                | T _ -> get_Nonterminals tl;;

(* Helper Function *)
let equal_second_elem_sets a b =
        let list1 = snd a in
        let list2 = snd b in
        equal_sets list1 list2;;

(* Helper Function *)
let rec get_new_symbols a =
        let (rules,reachable_symbols) = a in
        match rules with
                | [] -> (rules, reachable_symbols)
                | _ ->  let temp_rule = List.hd rules in
                        let rest_rules = List.tl rules in
                        let (curr_symbol, other_symbols) = temp_rule in
                        let nonterminal = get_Nonterminals other_symbols in
                        if List.mem curr_symbol reachable_symbols then get_new_symbols (rest_rules,(set_union reachable_symbols nonterminal)) 
                                                                else get_new_symbols (rest_rules, reachable_symbols);;

let rec get_reachable_symbols a =
        let (rules, reachable_symbols) = a in
        let newList = snd (get_new_symbols a) in
        if equal_sets newList reachable_symbols then (rules, reachable_symbols)
                                                else get_reachable_symbols (rules, newList);;

(* Helper Function *)
let rec filter_rules reachable_symbols rules = match rules with
        | [] -> []
        | hd::tl -> let curr_symbol = fst hd in
                if List.mem curr_symbol reachable_symbols then hd :: (filter_rules reachable_symbols tl)
                else (filter_rules reachable_symbols tl);;


let filter_reachable g =
        let (start_symbol, rules) = g in
        let (_,reachable_symbols) = computed_fixed_point equal_second_elem_sets get_reachable_symbols (rules,[start_symbol]) in
        let filtered = filter_rules reachable_symbols rules in
        (start_symbol, filtered);;
