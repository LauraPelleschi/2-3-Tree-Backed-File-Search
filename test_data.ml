open OUnit2
open Data
open Engine

module type Tests = sig
	val tests : OUnit2.test list
end



(* [DictTester] is where you will implement your test harness
 * to find buggy implementations. *)
module DictTester (M:DictionaryMaker) = struct


	module FormatString = struct
		type t = string
		let format fmt d =
			Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
	end

	module CompareString = struct
		include String

		let compare x y =
			match compare x y with
			| 0 -> `EQ
			| x -> if x>0 then `GT else `LT

		let format fmt d =
			Format.fprintf fmt "<abstr>"

	end

	module FormatInt = struct
		type t = int
		let format fmt d =
			Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
	end

	module CompareInt = struct
		type t=int

		let compare (x:int) (y:int) =
			match compare x y with
			| 0 -> `EQ
			| x -> if x>0 then `GT else `LT

		let format fmt d =
			Format.fprintf fmt "<abstr>"
	end




	module Dict_SS = M(CompareString)(FormatString)
	module Dict_II = M(CompareInt)(FormatInt)
	module Dict_SI = M(CompareString)(FormatInt)
	module Dict_IS = M(CompareInt)(FormatString)

 let dict_14_SS = Dict_SS.empty
                  |>Dict_SS.insert "k1" "v1"
                  |>Dict_SS.insert "k2" "v2"
                  |>Dict_SS.insert "k3" "v3"
                  |>Dict_SS.insert "k4" "v4"
                  |>Dict_SS.insert "k5" "v5"
                  |>Dict_SS.insert "k6" "v6"
                  |>Dict_SS.insert "k7" "v7"
                  |>Dict_SS.insert "k8" "v8"
                  |>Dict_SS.insert "k9" "v9"
                  |>Dict_SS.insert "k10" "10"
                  |>Dict_SS.insert "k11" "11"
                  |>Dict_SS.insert "k12" "k12"
                  |>Dict_SS.insert "k13" "k14"
                  |>Dict_SS.insert "k14" "v14"



 let dict_14_II = Dict_II.empty
                  |>Dict_II.insert 1 1
                  |>Dict_II.insert 2 2
                  |>Dict_II.insert 3 3
                  |>Dict_II.insert 4 4
                  |>Dict_II.insert 5 5
                  |>Dict_II.insert 6 6
                  |>Dict_II.insert 7 7
                  |>Dict_II.insert 8 8
                  |>Dict_II.insert 9 9
                  |>Dict_II.insert 10 10
                  |>Dict_II.insert 11 11
                  |>Dict_II.insert 12 12
                  |>Dict_II.insert 13 13
                  |>Dict_II.insert 14 14

 let choose_correct_SS = match Dict_SS.choose dict_14_SS with
   |None-> false
   |Some (x,y) when Dict_SS.find x dict_14_SS= Some y-> true
   |_->false

 let choose_correct_II = match Dict_II.choose dict_14_II with
   |None-> false
   |Some (x,y) when Dict_II.find x dict_14_II = Some y-> true
   |_->false


	let tests = [
		(*** The following are test cases for all functions the empty dictionary ***)
   "repok_empty_SS" >:: (fun _ -> assert_equal Dict_SS.empty (Dict_SS.rep_ok Dict_SS.empty));
   "repok_empty_II" >:: (fun _ -> assert_equal Dict_II.empty (Dict_II.rep_ok Dict_II.empty));
   "repok_empty_SI" >:: (fun _ -> assert_equal Dict_SI.empty (Dict_SI.rep_ok Dict_SI.empty));
   "repok_empty_IS" >:: (fun _ -> assert_equal Dict_IS.empty (Dict_IS.rep_ok Dict_IS.empty));

   "emptycheck_empty_SS" >:: (fun _ -> assert_equal true  (Dict_SS.is_empty Dict_SS.empty));
   "emptycheck_empty_II" >:: (fun _ -> assert_equal true  (Dict_II.is_empty Dict_II.empty));
   "emptycheck_empty_SI" >:: (fun _ -> assert_equal true  (Dict_SI.is_empty Dict_SI.empty));
   "emptycheck_empty_IS" >:: (fun _ -> assert_equal true  (Dict_IS.is_empty Dict_IS.empty));

   "size_empty_SS" >:: (fun _ -> assert_equal 0 (Dict_SS.size Dict_SS.empty));
   "size_empty_II" >:: (fun _ -> assert_equal 0 (Dict_II.size Dict_II.empty));
   "size_empty_SI" >:: (fun _ -> assert_equal 0 (Dict_SI.size Dict_SI.empty));
   "size_empty_IS" >:: (fun _ -> assert_equal 0 (Dict_IS.size Dict_IS.empty));

   "member_empty_SS" >:: (fun _ -> assert_equal false (Dict_SS.member "anystring" Dict_SS.empty));
   "member_empty_II" >:: (fun _ -> assert_equal false (Dict_II.member 69 Dict_II.empty));
   "member_empty_SI" >:: (fun _ -> assert_equal false (Dict_SI.member "anystring" Dict_SI.empty));
   "member_empty_IS" >:: (fun _ -> assert_equal false (Dict_IS.member 69 Dict_IS.empty));

   "find_empty_SS" >:: (fun _ -> assert_equal None (Dict_SS.find "anystring" Dict_SS.empty));
   "find_empty_II" >:: (fun _ -> assert_equal None (Dict_IS.find 69 Dict_IS.empty));
   "find_empty_SI" >:: (fun _ -> assert_equal None (Dict_SI.find "anystring" Dict_SI.empty));
   "find_empty_IS" >:: (fun _ -> assert_equal None (Dict_IS.find 69 Dict_IS.empty));

   "insert_empty_SS" >:: (fun _ -> assert_equal [("key","value")] (Dict_SS.to_list (Dict_SS.insert "key" "value" Dict_SS.empty)));
   "insert_empty_II" >:: (fun _ -> assert_equal [(6,9)] (Dict_II.to_list (Dict_II.insert 6 9 Dict_II.empty)));
   "insert_empty_SI" >:: (fun _ -> assert_equal [("key",9)] (Dict_SI.to_list (Dict_SI.insert "key" 9 Dict_SI.empty)));
   "insert_empty_IS" >:: (fun _ -> assert_equal [(6,"value")] (Dict_IS.to_list (Dict_IS.insert 6 "value" Dict_IS.empty)));

   "post_insert_empty_SS" >:: (fun _ -> assert_equal (Dict_SS.insert "key" "value" Dict_SS.empty) (Dict_SS.rep_ok (Dict_SS.insert "key" "value" Dict_SS.empty)));
   "post_insert_empty_II" >:: (fun _ -> assert_equal (Dict_II.insert 6 9 Dict_II.empty) (Dict_II.rep_ok (Dict_II.insert 6 9 Dict_II.empty)));



   "post_insert_empty_SI" >:: (fun _ -> assert_equal (Dict_SI.insert "key" 9 Dict_SI.empty) (Dict_SI.rep_ok (Dict_SI.insert "key" 9 Dict_SI.empty)));
   "post_insert_empty_IS" >:: (fun _ -> assert_equal (Dict_IS.insert 6 "value" Dict_IS.empty) (Dict_IS.rep_ok (Dict_IS.insert 6 "value" Dict_IS.empty)));

   (*)
   "remove_empty_SS" >:: (fun _ -> assert_equal  Dict_SS.empty (Dict_SS.remove "anystring" Dict_SS.empty));
   "remove_empty_II" >:: (fun _ -> assert_equal  Dict_II.empty (Dict_II.remove 69 Dict_II.empty));
   "remove_empty_SI" >:: (fun _ -> assert_equal  Dict_SI.empty (Dict_SI.remove "anystring" Dict_SI.empty));
   "remove_empty_IS" >:: (fun _ -> assert_equal  Dict_IS.empty (Dict_IS.remove 69 Dict_IS.empty));

   "post_remove_empty_SS" >:: (fun _ -> assert_equal  Dict_SS.empty ((Dict_SS.remove "anystring" Dict_SS.empty)|>Dict_SS.rep_ok));
   "post_remove_empty_II" >:: (fun _ -> assert_equal  Dict_II.empty ((Dict_II.remove 69 Dict_II.empty)|>Dict_II.rep_ok));
   "post_remove_empty_SI" >:: (fun _ -> assert_equal  Dict_SI.empty ((Dict_SI.remove "anystring" Dict_SI.empty)|>Dict_SI.rep_ok));
   "post_remove_empty_IS" >:: (fun _ -> assert_equal  Dict_IS.empty ((Dict_IS.remove 69 Dict_IS.empty)|>Dict_IS.rep_ok));
   *)

   "choose_empty_SS" >:: (fun _ -> assert_equal None (Dict_SS.choose Dict_SS.empty));
   "choose_empty_II" >:: (fun _ -> assert_equal None (Dict_II.choose Dict_II.empty));
   "choose_empty_SI" >:: (fun _ -> assert_equal None (Dict_SI.choose Dict_SI.empty));
   "choose_empty_IS" >:: (fun _ -> assert_equal None (Dict_IS.choose Dict_IS.empty));

   "fold_empty_SS" >:: (fun _ -> assert_equal  "" (Dict_SS.fold (fun k v accu->accu^k^v) "" Dict_SS.empty));
   "fold_empty_II" >:: (fun _ -> assert_equal  0 (Dict_II.fold (fun k v accu->accu+k+v) 0 Dict_II.empty));
   "fold_empty_SI" >:: (fun _ -> assert_equal  "" (Dict_SI.fold (fun k v accu->accu^k) "" Dict_SI.empty));
   "fold_empty_IS" >:: (fun _ -> assert_equal  0 (Dict_IS.fold (fun k v accu->accu+k) 0 Dict_IS.empty));

   "tolist_empty_SS" >:: (fun _ -> assert_equal [] (Dict_SS.to_list Dict_SS.empty));
   "tolist_empty_II" >:: (fun _ -> assert_equal [] (Dict_II.to_list Dict_II.empty));
   "tolist_empty_SI" >:: (fun _ -> assert_equal [] (Dict_SI.to_list Dict_SI.empty));
   "tolist_empty_IS" >:: (fun _ -> assert_equal [] (Dict_IS.to_list Dict_IS.empty));

   "expose_empty_SS" >:: (fun _ -> assert_equal Leaf (Dict_SS.expose_tree Dict_SS.empty));
   "expose_empty_II" >:: (fun _ -> assert_equal Leaf (Dict_II.expose_tree Dict_II.empty));
   "expose_empty_SI" >:: (fun _ -> assert_equal Leaf (Dict_SI.expose_tree Dict_SI.empty));
   "expose_empty_IS" >:: (fun _ -> assert_equal Leaf (Dict_IS.expose_tree Dict_IS.empty));



   (* Following cases for a dictionary of an arbitrary length 14, unique elements *)

   "repok_dict14_SS" >:: (fun _ -> assert_equal dict_14_SS (Dict_SS.rep_ok dict_14_SS));
   "repok_dict14_II" >:: (fun _ -> assert_equal dict_14_II (Dict_II.rep_ok dict_14_II));

   "emptycheck_dict14_SS" >:: (fun _ -> assert_equal false (Dict_SS.is_empty dict_14_SS));
   "emptycheck_dict14_II" >:: (fun _ -> assert_equal false  (Dict_II.is_empty dict_14_II));

   "size_dict14_SS" >:: (fun _ -> assert_equal 14 (Dict_SS.size dict_14_SS));
   "size_dict14_II" >:: (fun _ -> assert_equal 14 (Dict_II.size dict_14_II));


   "member_dict14_SS_True" >:: (fun _ -> assert_equal true (Dict_SS.member "k3" dict_14_SS));
   "member_dict14_II_True" >:: (fun _ -> assert_equal true (Dict_II.member 5 dict_14_II));



   "find_dict14_SS_True" >:: (fun _ -> assert_equal (Some "v9") (Dict_SS.find "k9" dict_14_SS));
   "find_dict14_II_True" >:: (fun _ -> assert_equal (Some 4) (Dict_II.find 4 dict_14_II));


   "insert_dict14_SS" >:: (fun _ -> assert_equal (Dict_SS.insert "key" "value" dict_14_SS) (Dict_SS.rep_ok (Dict_SS.insert "key" "value" dict_14_SS)));
   "insert_dict14_II" >:: (fun _ -> assert_equal (Dict_II.insert 69 69 dict_14_II) (Dict_II.rep_ok (Dict_II.insert 69 69 dict_14_II)));

   "remove_dict14_SS" >:: (fun _ -> assert_equal  (Dict_SS.remove "k3" dict_14_SS) (Dict_SS.rep_ok (Dict_SS.remove "k3" dict_14_SS)));
   "remove_dict14_II" >:: (fun _ -> assert_equal  (Dict_II.remove 5 dict_14_II) (Dict_II.rep_ok (Dict_II.remove 5 dict_14_II)));


   "choose_dict14_SS" >:: (fun _ -> assert_equal true (choose_correct_SS ));
   "choose_dict14_II" >:: (fun _ -> assert_equal true (choose_correct_II));

   "fold_dict14_SS" >:: (fun _ -> assert_equal  "k1k2k3k4k5k6k7k8k9k10k11k12k13k14" (Dict_SS.fold (fun k v accu->accu^k) "" dict_14_SS));
   "fold_dict_II" >:: (fun _ -> assert_equal  105 (Dict_II.fold (fun k v accu->accu+k+v) 0 dict_14_II));

   "tolist_dict14_SS" >:: (fun _ -> assert_equal [("k1","v1");("k2","v2");("k3","v3");
                                                  ("k4","v4");("k5","v5");("k6","v6");
                                                  ("k7","v7");("k8","v8");("k9","v9");
                                                  ("k10","v10");("k11","v11");
                                                  ("k12","v12");("k13","v13");
                                                  ("k14","v14")]
                              (List.sort compare (Dict_SS.to_list dict_14_SS)));



   (* operations arbitrary dictionary size 14 cannot perform making sure we get expected outcome*)
   "member_dict14_SS_False" >:: (fun _ -> assert_equal false (Dict_SS.member "anystring" dict_14_SS));
   "member_dict14_II_False" >:: (fun _ -> assert_equal false (Dict_II.member 69 dict_14_II));

   "find_dict14_SS_False" >:: (fun _ -> assert_equal None (Dict_SS.find "anystring" dict_14_SS));
   "find_dict14_II_False" >:: (fun _ -> assert_equal None (Dict_II.find 69 dict_14_II));

   (*Circumstances inserting a repeat or attempt to remove something not present *)
   "insert_dict14_SS" >:: (fun _ -> assert_equal dict_14_SS (Dict_SS.rep_ok (Dict_SS.insert "k1" "v1" dict_14_SS)));
   "insert_dict14_II" >:: (fun _ -> assert_equal dict_14_II (Dict_II.rep_ok (Dict_II.insert 1 1 dict_14_II)));
   (*)
   "remove_dict14_SS" >:: (fun _ -> assert_equal  dict_14_SS (Dict_SS.rep_ok (Dict_SS.remove "kjhkhk" dict_14_SS)));
   "remove_dict14_II" >:: (fun _ -> assert_equal  dict_14_II (Dict_II.rep_ok (Dict_II.remove 986654 dict_14_II)));
   *)


		(* Sets as list represented dictionaries*)


		(* Sets as 2 3 trees dictionaries *)


	]
end

module CompareChar = struct
	include Char

	let compare x y =
		match compare x y with
		| 0 -> `EQ
		| x -> if x>0 then `GT else `LT

	let format fmt d =
		Format.fprintf fmt "<abstr>"

end

module CompareInt = struct
	type t=int

	let compare (x:int) (y:int) =
		match compare x y with
		| 0 -> `EQ
		| x -> if x>0 then `GT else `LT

	let format fmt d =
		Format.fprintf fmt "<abstr>"
end

module Tree = MakeTreeDictionary(CompareInt)(CompareInt)

let one=Tree.insert 1 10 Tree.empty
let one_lst = [1,10]
let two=Tree.insert 5 50 one
let two_lst = [1,10;5,50]
let three = Tree.insert 3 30 two
let three_lst = [1,10;3,30;5,50]
let out_order= Tree.insert (-1) 12 three
let out_order_lst = [-1,12;1,10;3,30;5,50]
let product x y z = x*y +z
let sum_pro = Tree.fold (product) 0
let rem = Tree.remove 3 out_order
let t3110 = Tree.insert 1 3110 rem
let t3110_no_rem = Tree.insert 1 3110 two
let one'=Tree.insert 1 100 one


let test_tree = [

		"fold_empty" >:: (fun _ -> assert_equal  0 (sum_pro Tree.empty));
		"size_empty" >:: (fun _ -> assert_equal 0 (Tree.size Tree.empty));
    "repok_one" >:: (fun _ -> assert_equal one_lst (one|>Tree.rep_ok|>Tree.to_list));
  "find_empty" >:: (fun _ -> assert_equal None (Tree.find 1 Tree.empty));
		"tolist_empty" >:: (fun _ -> assert_equal [] (Tree.to_list Tree.empty));
		"choose_empty" >:: (fun _ -> assert_equal None (Tree.choose Tree.empty));

		"fold_one" >:: (fun _ -> assert_equal  10 (sum_pro one));
		"size_one" >:: (fun _ -> assert_equal 1 (Tree.size one));
  "repok_one" >:: (fun _ -> assert_equal one_lst (one|>Tree.rep_ok|>Tree.to_list));
		"find_one_y" >:: (fun _ -> assert_equal (Some 10) (Tree.find 1 one));
		"find_one_n" >:: (fun _ -> assert_equal (None) (Tree.find 3 one));
		"choose_one" >:: (fun _ -> assert_equal (Some (1,10)) (Tree.choose one));

		"fold_two" >:: (fun _ -> assert_equal  260 (sum_pro two));
		"size_two" >:: (fun _ -> assert_equal 2 (Tree.size two));
  "repok_two" >:: (fun _ -> assert_equal two_lst (two|>Tree.rep_ok|>Tree.to_list));
		"find_two_y" >:: (fun _ -> assert_equal (Some 10) (Tree.find 1 two));
		"find_two_n" >:: (fun _ -> assert_equal (None) (Tree.find 13 two));
  "tolist_two" >:: (fun _ -> assert_equal [1,10;5,50] (Tree.to_list two));

		"fold_three" >:: (fun _ -> assert_equal  350 (sum_pro three));
		"size_three" >:: (fun _ -> assert_equal 3 (Tree.size three));
  "repok_three" >:: (fun _ -> assert_equal three_lst (three|>Tree.rep_ok|>Tree.to_list));
  "tolist_three" >:: (fun _ -> assert_equal [(1,10);(3,30);(5,50)] (Tree.to_list three));


  "tolist_out_ord" >:: (fun _ -> assert_equal [(-1,12);(1,10);(3,30);(5,50)] (Tree.to_list out_order));
  "repok_out_ord" >:: (fun _ -> assert_equal out_order (Tree.rep_ok out_order));
  (*"out_ord_remo" >:: (fun _ -> assert_equal [(-1,12);(1,10);(5,50)] (Tree.to_list rem));
  "repok_rem" >:: (fun _ -> assert_equal rem (Tree.rep_ok rem));
  "rem_size" >:: (fun _ -> assert_equal 3 (Tree.size rem));
  "repok_rem" >:: (fun _ -> assert_equal rem (Tree.rep_ok rem));
    "rem_not_elt" >:: (fun _ -> assert_equal rem (Tree.remove 3 rem));
  "rem_not_elt_size" >:: (fun _ -> assert_equal 3 (Tree.size t3110));
    "insert_rep_elt_rem" >:: (fun _ -> assert_equal [(-1,12);(1,3110);(5,50)] (Tree.to_list t3110)); ; *)

  "insert_rep_size" >:: (fun _ -> assert_equal 2 (Tree.size t3110_no_rem));
  "insert_rep_lst" >:: (fun _ -> assert_equal [(1,3110);5,50] (Tree.to_list t3110_no_rem));
  "insert_rep_size" >:: (fun _ -> assert_equal 2 (Tree.size t3110_no_rem));


  "insert_rep_mem" >:: (fun _ -> assert_equal (Some 3110) (Tree.find 1 t3110_no_rem));
  "insert_rep_one" >:: (fun _ -> assert_equal [(1,100)] (Tree.to_list one'));
	]

module String1 = struct
  type t = string

  let compare x y =
    match compare x y with
    | 0 -> `EQ
    | x -> if x>0 then `GT else `LT

  let format fmt d =
    Format.fprintf fmt "%a" Format.pp_print_string d
end

module Int1 = struct
  type t = int

  let compare x y =
    match compare x y with
    | 0 -> `EQ
    | x -> if x>0 then `GT else `LT

  let format fmt d =
    Format.fprintf fmt "%a" Format.pp_print_int d
end




module TestList = DictTester(MakeListDictionary)
module TestTree = DictTester(MakeTreeDictionary)

module Sett= MakeSetOfDictionary(String1)(MakeListDictionary)

let empty=Sett.empty
let one=Sett.insert "hola" empty
let two = Sett.insert "que" one
let three = Sett.insert "pedo" two
let four = Sett.insert "locos" three
let four_lst = ["hola";"locos";"pedo";"que"]
let diff_two = empty|>Sett.insert "muriendo" |>Sett.insert "Sentimental"
let disj_uni = diff_two|>Sett.insert "que"|>Sett.insert "hola"
let disj_uni_lst = ["Sentimental";"hola";"muriendo";"que"]
let diff_four_disj = empty|>Sett.insert "pedo"|>Sett.insert "locos"
let diff_four_disj_lst = ["locos";"pedo"]
let rep = Sett.to_list


let set_tests = [
  "size_empty" >:: (fun _ -> assert_equal 0 (Sett.size empty));
  "repok_empty" >:: (fun _ -> assert_equal empty (Sett.rep_ok empty));
  "empty_isemtpy" >:: (fun _ -> assert_equal true (Sett.is_empty empty));
  "empty_mem" >:: (fun _ -> assert_equal false (Sett.member "hey" empty));
  "size_empty" >:: (fun _ -> assert_equal 0 (Sett.size empty));
  "union_empty" >:: (fun _ -> assert_equal empty (Sett.union empty empty));
  "choose_empty" >:: (fun _ -> assert_equal None (Sett.choose empty));
  "empty_diff" >:: (fun _ -> assert_equal empty (Sett.difference empty empty));
  "empty_fold" >:: (fun _ -> assert_equal "" (Sett.fold (^) "" empty));
  "empty_list" >:: (fun _ -> assert_equal [] (Sett.to_list empty));
  "empty_rem" >:: (fun _ -> assert_equal empty (Sett.remove "hey" empty));
  "empty_inter" >:: (fun _ -> assert_equal empty (Sett.intersect empty empty));

  "repok_one" >:: (fun _ -> assert_equal one (Sett.rep_ok one));
  "one_isemtpy" >:: (fun _ -> assert_equal false(Sett.is_empty one));
  "one_mem" >:: (fun _ -> assert_equal false (Sett.member "hey" one));
  "one_mem" >:: (fun _ -> assert_equal true (Sett.member "hola" one));
  "size_one" >:: (fun _ -> assert_equal 1 (Sett.size one));
  "union_one" >:: (fun _ -> assert_equal ["hola"] (one|>Sett.union one|>rep));
  "repok_four" >:: (fun _ -> assert_equal four_lst (four|>Sett.rep_ok|>rep));
  "one_diff" >:: (fun _ -> assert_equal empty (Sett.difference empty one));
  "one_diff2" >:: (fun _ -> assert_equal one (Sett.difference one empty));
  "empty_diff" >:: (fun _ -> assert_equal empty (Sett.difference one one));
  "one_fold" >:: (fun _ -> assert_equal "hola" (Sett.fold (^) "" one));
  "one_list" >:: (fun _ -> assert_equal ["hola"] (Sett.to_list one));
  "one_rem_empty" >:: (fun _ -> assert_equal one (Sett.remove "hey" one));
  "one_rem" >:: (fun _ -> assert_equal empty (Sett.remove "hola" one));
  "one_inter" >:: (fun _ -> assert_equal one (Sett.intersect one one));
  "One_empt_int" >:: (fun _ -> assert_equal empty (Sett.intersect empty one));
  "empt_one_inter" >:: (fun _ -> assert_equal empty (Sett.intersect one empty));

  "one_isemtpy" >:: (fun _ -> assert_equal false( Sett.is_empty four));
  "four_mem_f" >:: (fun _ -> assert_equal false (Sett.member "hey" four));
  "four_mem_t" >:: (fun _ -> assert_equal true (Sett.member "locos" four));
  "size_one" >:: (fun _ -> assert_equal 4 (Sett.size four));
  "union_disj" >:: (fun _ -> assert_equal disj_uni_lst ( diff_two|>Sett.union two|>rep));
  "union_same" >:: (fun _ -> assert_equal four_lst (four|>Sett.union four|>rep));
  "diff_not_disj" >:: (fun _ -> assert_equal diff_four_disj_lst ( disj_uni|>Sett.difference four|>rep));
]


module Sett2= MakeSetOfDictionary(Int1)(MakeTreeDictionary)

let empty=Sett2.empty
let one=Sett2.insert 1 empty
let two = Sett2.insert 4 one
let one' = Sett2.insert 5 empty
let two'= Sett2.insert (-1) one'
let three = Sett2.insert 1 two'
let union = empty|>Sett2.insert 1 |>Sett2.insert 4
let disj_uni = empty|>Sett2.insert (-1)|>Sett2.insert 1|>Sett2.insert 4|>Sett2.insert 5
let rep = Sett2.to_list

let tests1 = [

  "empty" >:: (fun _ -> assert_equal ([]) (rep empty));
  "two" >:: (fun _ -> assert_equal ([1;4]) (rep two));
  "three" >:: (fun _ -> assert_equal [-1;1;5] (rep three));
  "union" >:: (fun _ -> assert_equal [1;4] (one|>Sett2.union two|>rep));
  "dis_union" >:: (fun _ -> assert_equal [-1;1;4;5] (two|>Sett2.union two'|>rep));
  "inter" >:: (fun _ -> assert_equal empty (Sett2.intersect two two'));

]

(* [tests] is where you should provide OUnit test cases for
 * your own implementations of dictionaries and sets.  You're
 * free to use [DictTester] as part of that if you choose. *)
let tests = set_tests@test_tree@tests1 (*set_tests@TestList.tests@TestTree.tests@test_tree*)


(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)
