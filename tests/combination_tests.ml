open OUnit2
open Policy
module Example =
  struct
  type i = string
  type t = Empty | Elem of i
  type k = t list
  type f = t -> int -> t

  let i_to_string i = i
  let k_to_string k = let x = List.map (fun e -> match e with |Empty -> "ZERO" |Elem s -> s) k in String.concat "-" x
end
open Example
module ExampleCombination = Combination.StatesCombination(Example)
let f_null _ _ = Empty
let f1 e _ = 
  match e with
  | Empty -> Empty
  | Elem s -> Elem (s^"A")
let f2 e _ = 
  match e with
  | Empty -> Empty
  | Elem s -> Elem (s^"B")
let f3 e _ = 
  match e with
  | Empty -> Empty
  | Elem s -> Elem (s^"C")
let f4 e _ = 
  match e with
  | Empty -> Empty
  | Elem s -> Elem (s^"D")
let f5 e _ = 
  match e with
  | Empty -> Empty
  | Elem s -> Elem (s^"E")
let f6 e _ = 
  match e with
  | Empty -> Empty
  | Elem s -> Elem (s^"F")
let filter_fun = fun x -> x <> Empty ;;
let mf_m = 
[|
  [|[f_null];[f1;f2];[f_null]|];
  [|[f_null];[f_null];[f3;f4]|];
  [|[f5;f6];[f_null];[f_null]|]
|];;
module StringTools = Tools.Make(Example)
let multip_test_1 _ = 
  let ns = 3 in
  let mk0 = StringTools.make_init_state_matrix_singleton ~num_of_states:ns (Elem "")
  and exp_mk1_m = [| [Empty];[Elem "A";Elem "B"];[Empty]|]
  and mf = {ExampleCombination.matrix = mf_m; num_of_states=ns} in
  let mk1 = ExampleCombination.multiply mk0 mf 
  and exp_mk1 = {ExampleCombination.matrix = exp_mk1_m; step=1} in
  (*ExampleCombination.print_km mk1.matrix ;*)
  assert_equal exp_mk1 mk1
let multip_test_2 _ = 
  let ns = 3 
  and mk1_m = [| [Empty];[Elem "A";Elem "B"];[Empty]|] 
  and exp_mk2_m = [| [Empty;Empty;Empty;Empty;Empty];[Empty;Empty;Empty;Empty;Empty];[Empty;Elem "AC";Elem "BC";Elem "AD";Elem "BD";Empty]|] in
  let mk1 = StringTools.make_state_matrix mk1_m 1
  and mf = {ExampleCombination.matrix = mf_m; num_of_states=ns} in
  let mk2 = ExampleCombination.multiply mk1 mf 
  and exp_mk2 = {ExampleCombination.matrix = exp_mk2_m; step=2} in
  (*ExampleCombination.print_km mk2.matrix ;*)
  assert_equal exp_mk2 mk2

  let suite =
    "Test suite" >::: [
      "Multiplication test 1">:: multip_test_1;
      "Multiplication test 2">:: multip_test_2;
  ]

let () =
  run_test_tt_main suite