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

  end