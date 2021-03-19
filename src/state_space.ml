module type SS  = sig
    type state
    type state_trans_fun 
    type walk = state_trans_fun list
    (* situation is a state and history of actions leading to this this state *)
    type situation =  { current_state:state; current_walk:walk }
    (*type sit_trans_fun = situation -> state_trans_fun -> situation*)
    val advance_situation : situation -> state_trans_fun -> situation
end

module Make ( S : State.S ) = 
    struct
    type state = S.t
    type state_trans_fun = S.trans_fun 
    type walk = state_trans_fun list
    type situation =  { current_state:state; current_walk:walk }
    (*type sit_trans_fun = situation -> state_trans_fun -> situation*)
    let advance_situation situation trans_fun =
        let new_state = trans_fun.S.func situation.current_state 
        and new_walk = trans_fun :: situation.current_walk in
        { current_state=new_state; current_walk=new_walk }
end