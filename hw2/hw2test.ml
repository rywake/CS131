type awksub_nonterminals =
  |Expr | Term | Letter | Equal | Op | Num

let accept_all string = Some string;;
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x;;

let test_grammar =
  (Expr,
   function
     |  Expr ->
         [[N Term; N Op];
          [N Term]]
     | Term ->
	  [[N Num];
	  [N Equal];
	  [T"("; N Letter; T")"]]
     | Op ->
	 [[T"-"; N Num]; [T"+"; N Num]]
     | Letter ->
	 [[T"a"];
    [T"b"];
    [T"c"];
    [T"d"];
    [T"e"]]
     | Equal ->
	 [[T"="]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
    [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]]);;
    
let make_matcher_test_0 = make_matcher test_grammar accept_all ["(";"b";")";"+";"5";"3";"2"] = Some ["3";"2"];;
let make_matcher_test_1 = make_matcher test_grammar accept_empty_suffix ["(";"b";")";"+";"5";"3";"2"] = None;;

let make_parser_test_0 = make_parser test_grammar ["(";"b";")";"+";"5"] = Some (Node (Expr,
[Node (Term, [Leaf "("; Node (Letter, [Leaf "b"]); Leaf ")"]);
 Node (Op, [Leaf "+"; Node (Num, [Leaf "5"])])]));;

let make_parser_test_1 = match make_parser test_grammar ["(";"b";")";"+";"5"] with
|Some tree -> parse_tree_leaves tree = ["(";"b";")";"+";"5"]
|None -> false;;