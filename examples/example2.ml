open Policy
module SetOfActions = Set.Make(Action);;
module Example2 =
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
        |((a0,x0),(a1,x1)) ->
        "[" ^ string_of_int a0 ^ ":" ^ string_of_int x0 ^ "," ^ string_of_int a1 ^ ":" ^ string_of_int x1 ^ "]"
let k_to_string ks =
        List.fold_left
                (fun accu c ->
                        match c with
                        | Unreachable -> accu ^ ""
                        | Reachable (((a0,x0),(a1,x1)),set) -> accu ^ ( i_to_string ((a0,x0),(a1,x1)) ^ set_of_actions_2_string set )
                )
                ""
                ks
end
open Example2
let f0 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a0,x0+1),(a1,x1))
          and new_set = SetOfActions.add {label="0"; step=t+1} set in
                  Reachable (new_state, new_set)
let f1 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a1,x1+3),(a0,x0))
          and new_set = SetOfActions.add {label="1"; step=t+1} set in
                  Reachable (new_state, new_set)
let f2 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a0,x0+1),(a1,x1))
          and new_set = SetOfActions.add {label="2"; step=t+1} set in
                  Reachable (new_state, new_set)
let f3 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a1,x1+3),(a0,x0))
          and new_set = SetOfActions.add {label="3"; step=t+1} set in
                  Reachable (new_state, new_set)
let f4 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a0,x0+1),(a1,x1))
          and new_set = SetOfActions.add {label="4"; step=t+1} set in
                  Reachable (new_state, new_set)
let f5 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a1,x1),(a0,x0+3))
          and new_set = SetOfActions.add {label="5"; step=t+1} set in
                  Reachable (new_state, new_set)
let f6 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a0,x0),(a1,x1+1))
          and new_set = SetOfActions.add {label="6"; step=t+1} set in
                  Reachable (new_state, new_set)
let f7 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a1,x1),(a0,x0+3))
          and new_set = SetOfActions.add {label="7"; step=t+1} set in
                  Reachable (new_state, new_set)
let f8 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a1,x1+3),(a0,x0))
          and new_set = SetOfActions.add {label="8"; step=t+1} set in
                  Reachable (new_state, new_set)
let f9 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a0,x0+1),(a1,x1))
          and new_set = SetOfActions.add {label="9"; step=t+1} set in
                  Reachable (new_state, new_set)
let f10 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let synchro_agents = [x1;x0] in
          let condition,_ =  List.fold_left (fun (r,v0) v -> r && (v0 = v),v0 ) (true, List.hd synchro_agents) synchro_agents in 
          if condition then
                  let new_state = ((a1,x1+1),(a0,x0+1))
                  and new_set = SetOfActions.add {label="10"; step=t+1} set in
                  Reachable (new_state, new_set)
          else
                  Unreachable
let f11 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a0,x0),(a1,x1+1))
          and new_set = SetOfActions.add {label="11"; step=t+1} set in
                  Reachable (new_state, new_set)
let f12 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a0,x0),(a1,x1+1))
          and new_set = SetOfActions.add {label="12"; step=t+1} set in
                  Reachable (new_state, new_set)
let f13 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a1,x1),(a0,x0+3))
          and new_set = SetOfActions.add {label="13"; step=t+1} set in
                  Reachable (new_state, new_set)
let f14 e t =
  match e with
  | Unreachable -> Unreachable
  | Reachable (((a0,x0),(a1,x1)),set) ->
          let new_state = ((a1,x1),(a0,x0+3))
          and new_set = SetOfActions.add {label="14"; step=t+1} set in
                  Reachable (new_state, new_set)
let f_null _ _ = Unreachable
let mf_m = [|
  [|[f_null];[f0];[f1];[f_null];[f_null];[f_null]|];
  [|[f4];[f_null];[f_null];[f2];[f3];[f_null]|];
  [|[f7];[f_null];[f_null];[f5];[f6];[f_null]|];
  [|[f_null];[f9];[f8];[f_null];[f_null];[f_null]|];
  [|[f_null];[f14;f13];[f12;f11];[f_null];[f_null];[f10]|];
  [|[f_null];[f_null];[f_null];[f_null];[f_null];[f_null]|]
|]

let ns = 6
let mk0_m = Array.make 6 []
let init_state = (1,0),(2,0)
let init_set = SetOfActions.empty
let init_c_series = [ (Reachable (init_state,init_set) )]
let _ = Array.set mk0_m 0 init_c_series
module ExampleCombination = Combination.StatesCombination(Example2);;
let mk0 = {ExampleCombination.matrix = mk0_m;step=0};;
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
ExampleCombination.print_km mk4.matrix
let mk5 = ExampleCombination.multiply mk4 mf;;
print_endline "mk5";
ExampleCombination.print_km mk5.matrix;

       