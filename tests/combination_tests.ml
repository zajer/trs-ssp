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

let multip_test_1 _ = 
  let ns = 3 in
  let mk0_m = Array.make ns []
  and init_state = "" in
  let init_c_series = [ Elem init_state ] in 
  let _ = Array.set mk0_m 0 init_c_series 
  and exp_mk1_m = [| [Empty];[Elem "A";Elem "B"];[Empty]|]
  and mk0 = {ExampleCombination.matrix = mk0_m;step=0}
  and mf = {ExampleCombination.matrix = mf_m; num_of_states=ns} in
  let mk1 = ExampleCombination.multiply mk0 mf 
  and exp_mk1 = {ExampleCombination.matrix = exp_mk1_m; step=1} in
  let _ = assert_equal exp_mk1 mk1 in
  ExampleCombination.print_km mk1.matrix
  

  let suite =
    "Test suite" >::: [
      "Multiplication test 1">:: multip_test_1;
      
  ]

let () =
  run_test_tt_main suite