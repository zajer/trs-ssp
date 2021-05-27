namespace SSPLib

type trans_fun_raw = {permutationWithTimeShift:(int*int) list; reactLabel:string; fromIdx:int; toIdx:int; transitionIdx:int}
type dest_state = {state_idx:int;patts_found:string}
module State =
    type sat = (int*int) array
    type transFun = { func:sat->sat; transition_idx:int}
    type transFunConf = {perm_config: Map<int,(int*int)>;conditional_agents:int array}
    let private _numberOfAgents = ref (-1)
    let private _numberOfStates = ref (-1)
    let private _areValuesLocked = ref false
    let setValues lockValues numOfAgents numOfStates =
        if not !_areValuesLocked then
            _numberOfAgents := numOfAgents;
            _numberOfStates := numOfStates
        else
            raise (invalidOp "Attempt of changing locked state's values")
    let private _element_trans_fun input_rel_agent_id time_shift input_state =
        let (base_agent,base_time) = Array.get input_state (input_rel_agent_id-1)
        (base_agent,base_time+time_shift)
    let private _are_conditional_agents_synchronized cond_ags state =
        let agents_to_check = Array.map (fun i -> Array.get state (i-1)) cond_ags
        let _,reference_time = Array.get agents_to_check 0
        let result,_ = Array.fold (fun (synchro_flag,ref_time) (_,state) -> synchro_flag && ref_time = state, ref_time ) (true,reference_time) agents_to_check
        result
    let private _trans_fun_template config state = 
        if _are_conditional_agents_synchronized config.conditional_agents state then
            Array.init 
                !_numberOfAgents 
                (fun i -> 
                    let input_rel_agent_id, time_shift = Map.find (i+1) config.perm_config
                    _element_trans_fun input_rel_agent_id time_shift state 
                )
        else
            Array.init !_numberOfAgents (fun _ -> (-1,-1))
    let private _make_config raw_trans = 
        let cond_agents_list = ref []
        let perm,i = List.fold 
                        (
                            fun (perm_config,i) (aid,ts) -> 
                                (if ts > 0 then cond_agents_list := aid::!cond_agents_list);
                                Map.add (i+1) (aid,ts) perm_config, i+1
                        ) 
                        (Map.empty,0)
                        raw_trans.permutationWithTimeShift
        {perm_config = perm;conditional_agents = Array.ofList !cond_agents_list}
    let parseTransFun trans_raw =
        let config = _make_config trans_raw
        {func=_trans_fun_template config;transition_idx=trans_raw.transitionIdx}
    let numberOfStates () =
        !_numberOfStates
    let numberOfAgents () =
        !_numberOfAgents
    let isNegligible s =
        Array.forall ( fun (_,state) -> state = -1 ) s
    let toStirng state =
        let res = Array.map (fun (ai,state) -> "("+ai.ToString()+","+state.ToString()+")") state |> Array.toList |> String.concat ";"
        "{"+res+"}"
    let ofArray (a:(int*int) array) : sat =
        a