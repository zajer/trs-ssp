module type SS  = sig
    type state
    type state_trans_fun 
    type walk = state_trans_fun list
    type situation =  { current_state:state; current_walk:walk }
    
    val advance_situation : situation -> state_trans_fun -> situation
    val init_situation : state -> situation
end

module Make ( S : State.S) : SS 
    with type state = S.t and type state_trans_fun = S.trans_fun