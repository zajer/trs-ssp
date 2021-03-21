type t = (int*int)*(int*int)
type trans_fun = { func:t->t; transition_idx:int}

(* fun (a1,t1),(a2,t2) ->   *)
let trans_func_template_1 = (fun ((a1,t1),(a2,t2)) -> (a1,t1+2),(a2,t2)),[(1,2);(2,0)]
let trans_func_template_2 = (fun ((a1,t1),(a2,t2)) -> (a2,t2+1),(a1,t1)),[(2,1);(1,0)]
let trans_func_template_3 = (fun ((a1,t1),(a2,t2)) -> (a1,t1),(a2,t2+3)),[(1,0);(2,3)]

let templates = [trans_func_template_1;trans_func_template_2;trans_func_template_3]
let _match_permutation_with_template perm = 
    let res_template_func,_ =List.find (fun (_,templ_perm) -> List.equal (fun poi1 poi2 -> poi1 = poi2 ) perm templ_perm) templates in
    res_template_func


let parse_trans_fun trans_raw =
    let func = _match_permutation_with_template trans_raw.Policy.State.permutation_with_time_shift in
    {func;transition_idx=trans_raw.transition_idx}
let num_of_states () =
    3
    
