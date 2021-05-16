namespace SSPLib

module SS =
    type state = S.t
    type state_trans_fun = S.trans_fun 
    type walk = state_trans_fun list
    type situation =  { current_state:state; current_walk:walk }
    
    let advance_situation situation (trans_fun:state_trans_fun) =
        let new_state = trans_fun.func situation.current_state 
        let new_walk = trans_fun :: situation.current_walk
        { current_state=new_state; current_walk=new_walk }
    let init_situation state = 
        { current_state=state; current_walk=[] }
        
    