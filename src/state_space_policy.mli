module type S = sig
    type situation
    type state_trans_fun 
    type situations_in_state = Situations of situation Seq.t | Not_reachable
    type courses_between_situations = Courses of state_trans_fun Seq.t | No_transitions
    type system_situation_matrix = situations_in_state array
    type system_transformation_matrix = courses_between_situations Square_matrix.t
    
    val convolute : situations_in_state -> courses_between_situations -> situations_in_state
    val multiply : system_situation_matrix -> system_transformation_matrix -> system_situation_matrix
    val init_situation_in_state : situation -> situations_in_state
end

module Make ( S : State_space.SS) : S with type situation = S.situation and type state_trans_fun = S.state_trans_fun