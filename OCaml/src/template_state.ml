type t = (int*int) array
type trans_fun = { func:t->t; transition_idx:int}
type trans_fun_conf = {perm_config:(int,int*int)Hashtbl.t;conditional_agents:int array}

let _number_of_agents = ref (-1)
let _number_of_states = ref (-1)
let _element_trans_fun input_rel_agent_id time_shift input_state =
    let (base_agent,base_time) = Array.get input_state (input_rel_agent_id-1) in
    (base_agent,base_time+time_shift)
let _are_conditional_agents_synchronized cond_ags state =
    let agents_to_check = Array.map (fun i -> Array.get state (i-1)) cond_ags in
    let _,reference_time = Array.get agents_to_check 0 in
    let result,_ = Array.fold_left (fun (synchro_flag,ref_time) (_,t) -> synchro_flag && ref_time = t, ref_time ) (true,reference_time) agents_to_check in
    result
let _trans_fun_template config state = 
    if _are_conditional_agents_synchronized config.conditional_agents state then
        Array.init !_number_of_agents (fun i -> let input_rel_agent_id, time_shift = Hashtbl.find config.perm_config (i+1) in _element_trans_fun input_rel_agent_id time_shift state )
    else
        Array.init !_number_of_agents (fun _ -> (-1,-1))
let _make_config raw_trans = 
    let perm_config = Hashtbl.create (List.length raw_trans.State.permutation_with_time_shift) 
    and cond_agents_list = ref [] in
    List.iteri 
    (
        fun i (aid,ts) -> 
            (if ts > 0 then
            cond_agents_list := aid::!cond_agents_list);
            Hashtbl.add perm_config (i+1) (aid,ts)
    ) 
    raw_trans.permutation_with_time_shift ;
    {perm_config;conditional_agents=Array.of_list !cond_agents_list}
let parse_trans_fun trans_raw =
    let config = _make_config trans_raw in
    {func=_trans_fun_template config;transition_idx=trans_raw.transition_idx}
let num_of_states () =
    !_number_of_states
let is_negligible s =
    Array.for_all ( fun (_,t) -> t = -1 ) s

let to_stirng state =
    let res = Array.map (fun (ai,t) -> "("^(string_of_int ai)^","^(string_of_int t)^")") state |> Array.to_list |> String.concat ";" in
    "{"^res^"}"