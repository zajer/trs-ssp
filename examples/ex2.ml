open Ssp

module StateDef = struct
    type t = (int*int) array
    type trans_fun = { func:t->t; transition_idx:int}
    type trans_fun_conf = {perm_config:(int,int*int)Hashtbl.t;conditional_agents:int array}

    let _number_of_agents = 2
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
            Array.init _number_of_agents (fun i -> let input_rel_agent_id, time_shift = Hashtbl.find config.perm_config (i+1) in _element_trans_fun input_rel_agent_id time_shift state )
        else
            Array.init _number_of_agents (fun _ -> (-1,-1))
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
        6
    let is_negligible s =
        Array.for_all ( fun (_,t) -> t = -1 ) s
    
    let to_stirng state =
        let res = Array.map (fun (ai,t) -> "("^(string_of_int ai)^","^(string_of_int t)^")") state |> Array.to_list |> String.concat ";" in
        "{"^res^"}"
end

module Tools = Frontend.Make(StateDef)

let transitions_file = "trans_funs_ex2.csv"
let destination_states_file = "dest_states_ex1.csv"
let imported_trans_funs = Frontend.import_trans_funs transitions_file
let dest_states = Frontend.import_dest_states destination_states_file
let dest_state_idx = Frontend.destination_state_idx Frontend.FirstFound dest_states
let init_situation = [|(1,0);(2,0)|] |> Tools.SS.init_situation 
let max_num_of_steps = 777
let init_sm,tm = Tools.make_ssp_system imported_trans_funs init_situation ~state_idx:0 ~num_of_states:(StateDef.num_of_states ())
let situations_mx,num_of_steps,is_found = Tools.search_for_situation_in_state init_sm tm ~state_idx:dest_state_idx ~max_num_of_steps;;
(*Array.iteri
        (
            fun state_idx sits_in_state -> 
                match sits_in_state with
                | Tools.SSP.Not_reachable -> print_endline ((string_of_int state_idx)^": unreachable")
                | Situations s ->
                    let situations_list = List.of_seq s in
                    let content = List.map (fun sit -> StateDef.to_stirng sit.Tools.SS.current_state) situations_list |> String.concat "," in
                    print_endline ((string_of_int state_idx)^":"^content)
        )
        situations_mx;*)
if is_found then 
(
    (*print_endline ("Desired state with id="^(string_of_int dest_state_idx)^" found");*)
    let result_walk = Tools.walk_from_situation_matrix Frontend.FirstFound situations_mx dest_state_idx in
    let walk_to_save = Tools.export_walk result_walk imported_trans_funs in
    Frontend.export_trans_funs walk_to_save "ex2_result.csv"
)
else
(
    print_endline ("Desired state has not been reached with "^(string_of_int num_of_steps )^" steps!")

);;
