module type S =
sig
  type elt
  type t
  val make_init_state_matrix : num_of_states:int -> ?init_state_num:int -> elt list -> t
  val make_state_matrix : elt list array -> int -> t
end

module Make (Type:Combination.State) =
  struct
    module TypedCombination = Combination.StatesCombination (Type)
    type elt = Type.t
    type t = TypedCombination.k_matrix
    let make_init_state_matrix_singleton ~num_of_states ?(init_state_num=0) init_state_value =
      let res_m  = Array.make num_of_states [] in
      let _ = Array.set res_m init_state_num [init_state_value] in
      {TypedCombination.matrix = res_m;step=0}
    let make_init_state_matrix ~num_of_states ?(init_state_num=0) init_state_value =
      let res_m  = Array.make num_of_states [] in
      let _ = Array.set res_m init_state_num init_state_value in
      {TypedCombination.matrix = res_m;step=0}
    let make_state_matrix matrix  step=
      {TypedCombination.matrix = matrix;step}
    let is_state_reached state_id (k_matrix:t) = 
      let column = Array.get k_matrix.matrix state_id in
      match column with
      | [] -> false
      | _::_ -> true 
    let multiply_until_state_is_reached ?(filter_fun = fun _ -> true) ?(limit = -1 ) ~desired_state_id init_state f_matrix =
      let result = ref init_state 
      and is_reached = ref false 
      and iter = ref 1 in
        while not (!is_reached) || if limit <> -1 then false else !iter < limit do
          result := TypedCombination.multiply ~filter_fun !result f_matrix;
          is_reached := is_state_reached desired_state_id !result;
          iter := !iter +1
        done;
        !result,!is_reached
  end