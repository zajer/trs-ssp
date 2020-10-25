open Policy
module SetOfActions = Set.Make(Action);;
module Example =
  struct
  type i = (int*int)*(int*int)
  type t = Unreachable | Reachable of i*SetOfActions.t
  type k = t list
  type f = t -> int -> t

  let set_of_actions_2_string set = 
    let part1 = SetOfActions.fold (fun el sum -> (el.label^"^"^ string_of_int el.step ^"->")^sum ) set ""
    and part2 = "END" in
      "{"^part1 ^ part2^"}"

  let i_to_string (s:i) = 
    match s with
    | ((a,x),(b,y)) ->
    "[" ^ string_of_int a ^ ":" ^ string_of_int x ^ "," ^ string_of_int b ^ ":" ^ string_of_int y ^"]"
  let k_to_string ks =
    List.fold_left 
      (fun accu c -> 
        match c with 
        | Unreachable -> accu ^ ""
        | Reachable (((a,x),(b,y)),set) -> accu ^ ( i_to_string ((a,x),(b,y)) ^ set_of_actions_2_string set ) 
      ) 
      ""
      ks

end
module ExampleCombination = Combination.StatesCombination(Example);;
open Example
let f_null _ _ = Unreachable
let f1 e t = 
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a,x),(b,y)),set) -> 
    let new_state = ((a,x+1),(b,y)) 
    and new_set = SetOfActions.add {label="1"; step=t+1} set
    in
      Reachable (new_state, new_set)
let f2 e t = 
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a,x),(b,y)),set) -> 
    let new_state = ((b,y+1),(a,x)) 
    and new_set = SetOfActions.add {label="2"; step=t+1} set
    in
      Reachable (new_state, new_set)
let f3 e t = 
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a,x),(b,y)),set) -> 
    let new_state = ((a,x+1),(b,y)) 
    and new_set = SetOfActions.add {label="3"; step=t+1} set
    in
      Reachable (new_state, new_set)
let f4 e t = 
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a,x),(b,y)),set) -> 
    let new_state = ((a,x),(b,y+1)) 
    and new_set = SetOfActions.add {label="4"; step=t+1} set
    in
      Reachable (new_state, new_set)
let f5 e t = 
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a,x),(b,y)),set) -> 
    let new_state = ((a,x),(b,y+1)) 
    and new_set = SetOfActions.add {label="5"; step=t+1} set
    in
      Reachable (new_state, new_set)
let f6 e t = 
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a,x),(b,y)),set) -> 
    let new_state = ((a,x+1),(b,y)) 
    and new_set = SetOfActions.add {label="6"; step=t+1} set
    in
      Reachable (new_state, new_set)
let f7 e t = 
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a,x),(b,y)),set) -> 
    let new_state = ((b,y+1),(a,x)) 
    and new_set = SetOfActions.add {label="7"; step=t+1} set
    in
      Reachable (new_state, new_set)
let f8 e t = 
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a,x),(b,y)),set) -> 
    let new_state = ((a,x),(b,y+1)) 
    and new_set = SetOfActions.add {label="8"; step=t+1} set
    in
      Reachable (new_state, new_set)


let ns = 6
let mk0_m = Array.make 6 []
let init_state = (1,0),(2,0)
let init_set = SetOfActions.empty
let init_c_series = [ (Reachable (init_state,init_set) )]
let _ = Array.set mk0_m 0 init_c_series

let mk0 = {ExampleCombination.matrix = mk0_m;step=0}

let mf_m = 
  [|
    [|[f_null];[f1;f2];[f_null];[f_null];[f_null];[f_null]|];
    [|[f_null];[f_null];[f3];[f4];[f_null];[f_null]|];
    [|[f_null];[f_null];[f_null];[f_null];[f5];[f_null]|];
    [|[f_null];[f_null];[f_null];[f_null];[f6;f7];[f_null]|];
    [|[f_null];[f_null];[f_null];[f_null];[f_null];[f8]|];
    [|[f_null];[f_null];[f_null];[f_null];[f_null];[f_null]|]
  |]

let mf = {ExampleCombination.matrix = mf_m; num_of_states=ns};;

print_endline "printing time!";
print_endline "mk0";
ExampleCombination.print_km mk0.matrix;;
let mk1 = ExampleCombination.multiply mk0 mf;;
print_endline "mk1";
ExampleCombination.print_km mk1.matrix
let mk2 = ExampleCombination.multiply mk1 mf;;
print_endline "mk2";
ExampleCombination.print_km mk2.matrix
let mk3 = ExampleCombination.multiply mk2 mf;;
print_endline "mk3";
ExampleCombination.print_km mk3.matrix
let mk4 = ExampleCombination.multiply mk3 mf;;
print_endline "mk4";
ExampleCombination.print_km mk4.matrix;;
module TypedTools = Tools.Make(Example)
let res,is_reached = TypedTools.multiply_until_state_is_reached 
  ~filter_fun:(fun x -> if x <> Unreachable then true else false) 
  ~limit:777
  ~desired_state_id:5
  mk0
  mf;;
print_endline "first reach of 5th state";
ExampleCombination.print_km res.matrix