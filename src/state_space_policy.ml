
module type SSP = sig
    type situation
    type state_trans_fun 
    type situations_in_state = Situations of situation Seq.t | Not_reachable
    type courses_between_situations = Courses of state_trans_fun Seq.t | No_transitions
    (*
    type system_situation_matrix = situation Seq.t array
    type system_transformation_matrix = state_trans_fun Seq.t array array
    *)
    type system_situation_matrix = situations_in_state array
    type system_transformation_matrix = courses_between_situations Square_matrix.t
    (*
    val convolute : situation Seq.t -> state_trans_fun Seq.t -> int -> situation
    val multiply : system_situation_matrix -> system_situation_matrix -> system_situation_matrix
    *)
    val convolute : (situation-> situation Option.t) -> situations_in_state -> courses_between_situations -> situations_in_state
    val multiply : (situation-> situation Option.t) -> system_situation_matrix -> system_transformation_matrix -> system_situation_matrix
    val init_situation_in_state : situation -> situations_in_state
    val init_situation_matrix : situations_in_state -> state_idx:int -> num_of_states:int -> system_situation_matrix
end

module Make ( SS : State_space.SS) =
struct
    type situation = SS.situation 
    type state_trans_fun = SS.state_trans_fun
    type situations_in_state = Situations of situation Seq.t | Not_reachable
    type courses_between_situations = Courses of state_trans_fun Seq.t | No_transitions
    type system_situation_matrix = situations_in_state array
    type system_transformation_matrix = courses_between_situations Square_matrix.t

    let convolute filtering_fun situations  trans_funs = 
        match situations,trans_funs with
        | Not_reachable,No_transitions -> Not_reachable
        | Situations _, No_transitions -> Not_reachable
        | Not_reachable, Courses _ -> Not_reachable
        | Situations sits, Courses trans_funs -> 
            let result = Seq.flat_map 
                (
                    fun sit -> 
                        let new_situations = Seq.filter_map (fun trans_fun -> SS.advance_situation sit trans_fun |> filtering_fun) 
                            trans_funs
                            in
                            new_situations
                )
                sits
            in
            Situations result
    let _merge_situations_at_state sits1 sits2 = 
        match sits1, sits2 with
        | Not_reachable, Not_reachable -> Not_reachable
        | Situations s1, Not_reachable -> Situations s1
        | Not_reachable, Situations s2 -> Situations s2
        | Situations s1, Situations s2 -> Situations (Seq.append s1 s2)
    let multiply filtering_fun situations_mx trans_mx =
        let result = Array.mapi 
        (
            fun result_column_id _ -> 
                let column_of_functions = Square_matrix.column trans_mx result_column_id in
                let new_situations_in_state_to_flatten = 
                    Array.mapi 
                        (
                            fun state_id situations_in_state -> convolute filtering_fun situations_in_state (Array.get column_of_functions state_id)
                        ) 
                        situations_mx 
                    in
                let new_situations_in_state = Array.fold_left 
                    (fun res_situation sits_to_merge -> _merge_situations_at_state res_situation sits_to_merge ) 
                    Not_reachable 
                    new_situations_in_state_to_flatten
                in
                    new_situations_in_state
        ) 
        (Array.make trans_mx.length [])
        in
            result
    let init_situation_in_state (sit:situation) =
        let res =  Seq.cons sit (Seq.empty)
        in
            (Situations res)
    let init_situation_matrix sit_in_state ~state_idx ~num_of_states =
        let result = Array.make num_of_states Not_reachable in
        Array.set result state_idx sit_in_state;
        result
end