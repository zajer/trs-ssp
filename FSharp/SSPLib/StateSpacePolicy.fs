namespace SSPLib

module SSP =
    type situation = SS.situation 
    type state_trans_fun = SS.state_trans_fun
    type situations_in_state = Situations of seq<situation> | Not_reachable
    type courses_between_situations = Courses of seq<state_trans_fun> | No_transitions
    type system_situation_matrix = situations_in_state array
    type system_transformation_matrix = courses_between_situations SquareMatrix.t

    let convolute filtering_fun situations  trans_funs = 
        match situations,trans_funs with
        | Not_reachable,No_transitions -> Not_reachable
        | Situations _, No_transitions -> Not_reachable
        | Not_reachable, Courses _ -> Not_reachable
        | Situations sits, Courses trans_funs -> 
            let result = Seq.collect
                            (
                                fun sit -> 
                                    let new_situations = Seq.choose 
                                                            (fun trans_fun -> SS.advance_situation sit trans_fun |> filtering_fun) 
                                                            trans_funs
                                    new_situations
                            )
                            sits
            Situations result
    let _merge_situations_at_state sits1 sits2 = 
        match sits1, sits2 with
        | Not_reachable, Not_reachable -> Not_reachable
        | Situations s1, Not_reachable -> Situations s1
        | Not_reachable, Situations s2 -> Situations s2
        | Situations s1, Situations s2 -> Situations (Seq.append s1 s2)
    let multiply filtering_fun situations_mx trans_mx =
        let result = Array.Parallel.mapi 
                        (
                            fun to_state_id _ -> 
                                let column_of_functions_to_state = SquareMatrix.column trans_mx to_state_id
                                let new_situations_in_state_to_flatten = 
                                    Array.mapi 
                                        (
                                            fun from_state_id situations_in_state ->
                                                let transitions_from_state_id = (Array.get column_of_functions_to_state from_state_id)
                                                convolute 
                                                    filtering_fun 
                                                    situations_in_state 
                                                    transitions_from_state_id
                                        ) 
                                        situations_mx 
                                let new_situations_in_state = Array.fold 
                                                                (fun res_situation sits_to_merge -> _merge_situations_at_state res_situation sits_to_merge ) 
                                                                Not_reachable 
                                                                new_situations_in_state_to_flatten
                                new_situations_in_state
                        ) 
                        (Array.create trans_mx.length [])
        result
    let init_situation_in_state (sit:situation) =
        let res =  Seq.singleton sit
        (Situations res)
    let init_situation_matrix sit_in_state state_idx num_of_states =
        let result = Array.create num_of_states Not_reachable
        Array.set result state_idx sit_in_state;
        result
