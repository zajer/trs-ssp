namespace SSPLib

module StateSpacePolicy =
    type situationsInState = Situations of seq<StateSpace.situation> | Not_reachable
    type coursesBetweenSituations = Courses of seq<State.transFun> | No_transitions
    type systemSituationMatrix = situationsInState array
    type systemTransformationMatrix = coursesBetweenSituations SquareMatrix.matrix

    let convolute filtering_fun situations  courses = 
        match situations,courses with
        | Not_reachable,No_transitions -> Not_reachable
        | Situations _, No_transitions -> Not_reachable
        | Not_reachable, Courses _ -> Not_reachable
        | Situations sits, Courses trans_funs -> 
            let result = Seq.collect
                            (
                                fun sit -> 
                                    let new_situations = Seq.choose 
                                                            (fun trans_fun -> StateSpace.advanceSituation sit trans_fun |> filtering_fun) 
                                                            trans_funs
                                    new_situations
                            )
                            sits
            Situations result
    let private _mergeSituationsAtState sits1 sits2 = 
        match sits1, sits2 with
        | Not_reachable, Not_reachable -> Not_reachable
        | Situations s1, Not_reachable -> Situations s1
        | Not_reachable, Situations s2 -> Situations s2
        | Situations s1, Situations s2 -> Situations (Seq.append s1 s2)
    type multiplicationConfig = 
        {   
            limit:bool;
            chunkSize:int;
            filteringFunc:StateSpace.situation->option<StateSpace.situation>
        }
    let private _limitSituationsAt_state chunkSize allSituationsAtState =
        match allSituationsAtState with
        | Not_reachable -> Not_reachable
        | Situations sits -> Situations (Seq.truncate chunkSize sits)
    let private _multiply config situationsMX transMX =
        Array.Parallel.mapi 
            (
                fun to_state_id _ ->
                    let column_of_functions_to_state = SquareMatrix.column transMX to_state_id
                    let new_situations_in_state_to_flatten =
                        Array.mapi
                            (
                                fun from_state_id situations_in_state ->
                                    let transitions_from_state_id = (Array.get column_of_functions_to_state from_state_id)
                                    convolute
                                        config.filteringFunc
                                        situations_in_state
                                        transitions_from_state_id
                            )
                            situationsMX
                    let new_situations_in_state = Array.fold
                                                    (fun res_situation sits_to_merge -> _mergeSituationsAtState res_situation sits_to_merge ) 
                                                    Not_reachable 
                                                    new_situations_in_state_to_flatten
                    if config.limit then
                        new_situations_in_state |> _limitSituationsAt_state config.chunkSize
                    else
                        new_situations_in_state
            )
            (Array.create transMX.length [])
    let multiply filteringFun (situationsMX:systemSituationMatrix) transMX =
        let config = {limit=false;chunkSize=(-1);filteringFunc=filteringFun}
        _multiply config situationsMX transMX 
    let multiplyLimited filteringFun (situationsMX:systemSituationMatrix) transMX maxResSize = 
        let config = {limit=true;chunkSize=maxResSize;filteringFunc=filteringFun}
        _multiply config situationsMX transMX
    let initSituationInState (sit:StateSpace.situation) =
        let res =  Seq.singleton sit
        (Situations res)
    let initSituationMatrix sis initStateIdx numOfStates =
        let result = Array.create numOfStates Not_reachable
        Array.set result initStateIdx sis;
        result
