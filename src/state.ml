type trans_fun_raw = {permutation_with_time_shift:(int*int) list; react_label:string; from_idx:int; to_idx:int; transition_idx:int}

module type S = sig
    type t
    type trans_fun = { func:t->t; transition_idx:int}
    
    val parse_trans_fun : trans_fun_raw -> trans_fun
end

