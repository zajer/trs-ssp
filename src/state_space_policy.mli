module type SSP = sig
    type situation
    type state_trans_fun 
    type situations_in_state = Situations of situation Seq.t | Not_reachable
    type courses_between_situations = Courses of state_trans_fun Seq.t | No_transitions
    type system_situation_matrix = situations_in_state array
    type system_transformation_matrix = courses_between_situations Square_matrix.t
    
    val convolute : (situation -> situation Option.t) -> situations_in_state -> courses_between_situations -> situations_in_state
    val multiply : (situation-> situation Option.t) -> system_situation_matrix -> system_transformation_matrix -> system_situation_matrix
    val init_situation_in_state : situation -> situations_in_state
    val init_situation_matrix : situations_in_state -> state_idx:int -> num_of_states:int -> system_situation_matrix
end

module Make ( SS : State_space.SS) : SSP with type situation = SS.situation and type state_trans_fun = SS.state_trans_fun