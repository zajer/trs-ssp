type t = (int*int)*(int*int)
type trans_fun = { func:t->t; transition_idx:int}

let parse_trans_fun _ =
    {func=(fun s -> s); transition_idx=(-1)}
let num_of_states () = 2