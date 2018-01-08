open Data

exception Unimplemented

(*New function*)
let flip_f f x y = f y x

module type Engine = sig
	type idx
	val index_of_dir : string -> idx
	val to_list : idx -> (string * string list) list
	val or_not  : idx -> string list -> string list -> string list
	val and_not : idx -> string list -> string list -> string list
	val format : Format.formatter -> idx -> unit
end

module MakeEngine
	(S:Data.Set with type Elt.t = string)
	(D:Data.Dictionary with type Key.t = string and type Value.t = S.t)
	: Engine
=
struct
	type idx = D.t

(* [list_files accu handle] is the list of filenames ending in '.txt' in the
 * directory corresponding to [handle] appended to [accu]
 * requires: [accu]:string list *)
	let rec list_files accu handle =
		let exp = Str.regexp ".+\\.txt$" in
		try
			let file = handle|>Unix.readdir in
   if Str.string_match exp file 0 then (* if filename ends in '.txt' *)
				list_files (file::accu) handle
			else
				list_files accu handle
		with
		| End_of_file -> accu

(* [to_word word] is the maximal length valid word, according to the definition
 * of word in the A3 writeup, contained in [word]
 * requires: [word] has no whitespaces *)
	let to_word word =
		let open Str in
		let exp = "[A-Za-z0-9]"|>regexp in
		try
		let ini=Str.search_forward exp word 0 in
    let last=Str.search_backward exp word (String.length word-1) in
    let leng = 1+last-ini in
    Some (String.sub word ini leng|>String.lowercase_ascii)
		with
  | Not_found -> None

(* [to_word word] is the maximal length valid word, according to the definition
 * of word in the A3 writeup, contained in [word]
 * requires: [word] has no whitespaces *)
	let to_word' word =
		let open Str in
		let exp = "[A-Za-z0-9]"|>regexp in
		try
		let ini=Str.search_forward exp word 0 in
    let last=Str.search_backward exp word (String.length word-1) in
    let leng = 1+last-ini in
    Some (String.sub word ini leng|>String.lowercase_ascii)
		with
		| Not_found -> None

(* [filt_word accu lst] is the list of all x such that Some x is in [lst]
 * appended to [accu].
 * requires: if [accu]:t list, then [lst]: t option list  *)
	let rec filt_word accu = function
		| [] -> accu
		| (Some x)::t -> filt_word (x::accu) t
		| None::t -> filt_word accu t

(* [parse channel accu 0] returns a list of words in the file corresponding to
 * [channel] appended to [accu]
 * requires: accu: string list, no functions have been applied to [channel] *)
	let rec parse_help channel accu count =
   try
     let lst =
		channel
		|> input_line
		|> String.split_on_char ' '
		|> List.rev_map to_word
		|> filt_word []
    |> List.rev_append accu
     in if count mod 10 = 0 then
       parse_help channel (List.sort_uniq compare lst) 0
      else
        parse_help channel lst (count+1)
		with
  | End_of_file -> List.sort_uniq compare accu

  (* [parse channel accu 0] returns a list of words in the file corresponding to
  * [channel] appended to [accu]
  * requires: accu: string list, no functions have been applied to [channel] *)
  let rec parse_help' channel accu =
   try
  	channel
  	|> input_line
  	|> String.split_on_char ' '
  	|> List.rev_map to_word
  	|> filt_word []
    |> List.fold_left (fun x y -> S.insert y x) accu
    |> parse_help' channel
  	with
    | End_of_file -> accu

  (* [parse channel accu] returns a list of words in the file corresponding to
   * [channel] appended to [accu]
   * requires: accu: string list, no functions have been applied to [channel] *)
  let rec parse channel accu =
    parse_help channel accu 0

  (* [parse channel accu] returns a list of words in the file corresponding to
  * [channel] appended to [accu]
  * requires: accu: string list, no functions have been applied to [channel] *)
  let rec parse' channel accu =
    parse_help' channel accu

 (* [from_option opt] is the value inside the option [opt].
  * requires: [opt] is not None *)
  let from_option = function
    | None -> failwith "None"
    | Some x -> x

  (* [into_dict words file accu] is a dictionary containing all the elements in
   * [words] as keys with [file] now included in the set that coresponds to them.
   * requires: [accu] is the empty dictionary; [words]: string list*)
  let rec into_dict words file accu =
    match words with
    | [] -> accu
    | h::t ->
    	begin
    		let value = D.find h accu in
    		if value = None then
    			D.insert h (S.empty|>S.insert file) accu |> into_dict t file
    		else
    			D.insert h (value|>from_option|>S.insert file) accu |> into_dict t file
    	 end

  (* [rev_f1 f x (y,z)] is the tuple (y,f z x) *)
  let rev_f1 f x (y,z) = (y,f z x)

  (* [rev_f2 f x (y,z)] is f z y x *)
  let rev_f2 f x (y,z) = f z y x


  let index_of_dir d =
    d
    |> Unix.opendir
    |> list_files []
    |> List.rev_map (fun x -> (x,d^Filename.dir_sep^x |> open_in))
    |> List.rev_map (fun (y,z) -> (y,parse' z S.empty|>S.to_list))
    |> List.fold_left (fun x (y,z) -> into_dict z y x) D.empty

	let to_list idx =
		idx
		|> D.to_list
		|> List.rev_map (fun (x,y)->(x,S.to_list y))

  (* [opt accu option] is x where x is the value in the option, or [accu]
   * if the option is none*)
  let opt accu = function
  	| None -> accu
  	| Some x -> x

  	(*[contain idx word] is a list of *)
   let contain idx word =
     D.find (word|>String.lowercase_ascii) idx|> opt S.empty |> S.to_list

   (*[contain idx word] is a list of *)
   let contain_s idx word =
     D.find (word|>String.lowercase_ascii) idx|> opt S.empty

   (* New function*)
   let filter (p) (s) =
    let add x set = if p x then S.insert x set else set in
    S.fold add S.empty s

  (*New function*)
  let rec and_or_not idx boo words accu=
  	let f head lst = (contain idx head|>flip_f List.mem) lst |> boo in
  	match words with
  	| [] -> accu|>List.sort_uniq compare
    | h::t -> and_or_not idx boo t (List.filter (f h) accu)

  let rec and_or_not_s idx boo words accu=
   let f head elt_acc = (contain_s idx head|>flip_f S.member) elt_acc |> boo in
   match words with
   | [] -> accu
   | h::t -> and_or_not_s idx boo t (filter (f h) accu)

  (* It returns all the files that contain any of [words] *)
  let rec orss idx words accu =
  	match words with
  	| [] -> accu|>List.sort_uniq compare
    | h::t -> orss idx t (accu@contain idx h)

  (* It returns all the files that contain any of [words]*)
  let rec orss_s idx words accu =
   match words with
   | [] -> accu
   | h::t -> orss_s idx t (h|>contain_s idx|> S.union accu)

	let or_not_ss idx ors nots =
		orss idx ors []
    |> and_or_not idx (not) nots

  let or_not idx ors nots =
		orss_s idx ors S.empty
    |> and_or_not_s idx (not) nots
    |> S.to_list

	let and_not_ss idx ands nots =
		ands
		|> List.hd
		|> contain_s idx
		|> and_or_not_s idx (fun x -> x) ands
    |> and_or_not_s idx (not) nots
    |>S.to_list

  let and_not idx ands nots =
  	ands
  	|> List.hd
  	|> contain_s idx
  	|> and_or_not_s idx (fun x -> x) ands
    |> and_or_not_s idx (not) nots
    |> S.to_list

	let format fmt idx =
   D.format fmt idx
end

module TrivialEngine =
struct
	type idx = unit
	let index_of_dir d = ()
	let to_list idx = []
	let or_not idx ors nots = []
	let and_not idx ands nots = []
	let format fmt idx = ()
end

module String1 = struct
  include String

  let compare x y =
    match compare x y with
    | 0 -> `EQ
    | x -> if x>0 then `GT else `LT

  let format fmt d =
    Format.fprintf fmt "%a" Format.pp_print_string d
end

module SetList = MakeSetOfDictionary(String1)(MakeListDictionary)
module SetTree = MakeSetOfDictionary(String1)(MakeTreeDictionary)
module DictList = MakeListDictionary(String1)(SetList)
module DictTree = MakeTreeDictionary(String1)(SetTree)

module ListEngine = MakeEngine(SetList)(DictList)

module TreeEngine = MakeEngine(SetTree)(DictTree)
