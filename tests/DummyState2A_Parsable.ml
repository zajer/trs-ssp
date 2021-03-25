open Ssp
type atom = (int*int)*(int*int) 
type t = A of atom | Null
type trans_fun = { func:t->t; transition_idx:int}

let fun_1 = fun s -> match s with | A ((a1,t1),(a2,t2)) -> A ((a1,t1+2),(a2,t2)) | Null -> Null
let fun_2 = fun s -> match s with | A ((a1,t1),(a2,t2)) -> A ((a2,t2+1),(a1,t1)) | Null -> Null
let fun_3 = fun s -> match s with | A ((a1,t1),(a2,t2)) -> A ((a1,t1),(a2,t2+3)) | Null -> Null
let fun_4 = 
    fun s ->
    match s with 
    | A ((a1,t1),(a2,t2)) ->
        if t1 = t2 then 
            A ((a2,t2+2),(a1,t1+2))
        else
            Null
    | Null -> Null
let trans_func_template_1 = fun_1,[(1,2);(2,0)]
let trans_func_template_2 = fun_2,[(2,1);(1,0)]
let trans_func_template_3 = fun_3,[(1,0);(2,3)]
let trans_func_template_4 = fun_4,[(2,2);(1,2)]

let templates = [trans_func_template_1;trans_func_template_2;trans_func_template_3;trans_func_template_4]
let _match_permutation_with_template perm = 
    let res_template_func,_ = List.find (fun (_,templ_perm) -> List.equal (fun poi1 poi2 -> poi1 = poi2 ) perm templ_perm) templates in
    res_template_func


let parse_trans_fun trans_raw =
    let func = _match_permutation_with_template trans_raw.State.permutation_with_time_shift in
    {func;transition_idx=trans_raw.transition_idx}

let num_of_states () =
    3
let is_negligible state =
    match state with
    | A _ -> false
    | Null -> true
    
