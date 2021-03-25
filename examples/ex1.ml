open Policy

module StateDef = struct
   type t = (int*int) array
    type trans_fun = { func:t->t; transition_idx:int}
    let number_of_agents = 2

    let trans_fun_template output_config state = 
        let input_config = Hashtbl.create number_of_agents in
        Array.iteri 
            (fun i (aid,t) -> Hashtbl.add input_config (i+1) (aid,t) ) 
            state ;
        Array.init number_of_agents
            (
                fun i ->
                    let output_relative_agent_id, output_time_shift = Hashtbl.find output_config (i+1) in
                    let output_agent_id, input_time = Hashtbl.find input_config output_relative_agent_id in
                    output_agent_id,input_time+output_time_shift
            ) 
            
    let parse_trans_fun trans_raw =
        let config = Hashtbl.create (List.length trans_raw.State.permutation_with_time_shift) in
        List.iteri (fun i (aid,tsh) -> Hashtbl.add config (i+1) (aid,tsh)) trans_raw.permutation_with_time_shift;
        {func=trans_fun_template config;transition_idx=trans_raw.transition_idx}
    let num_of_states () =
        6
    let is_negligible s =
        Array.for_all ( fun (_,t) -> t = -1 ) s
    
    let to_stirng state =
        let res = Array.map (fun (ai,t) -> "("^(string_of_int ai)^","^(string_of_int t)^")") state |> Array.to_list |> String.concat ";" in
        "{"^res^"}"
end

module Tools = Frontend.Make(StateDef)

let transitions_file = "trans_funs_ex1.csv"
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
    Frontend.export_trans_funs walk_to_save "ex1_result.csv"
)
else
(
    print_endline ("Desired state has not been reached with "^(string_of_int num_of_steps )^" steps!")

);;
