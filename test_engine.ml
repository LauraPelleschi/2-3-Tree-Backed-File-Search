open OUnit2
open Engine
open Data

let test1 = Unix.getcwd () ^ Filename.dir_sep ^ "test1"
let idx = test1 |> ListEngine.index_of_dir
let med = "medium.txt"
let small = "small.txt"





let tests = []

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)
