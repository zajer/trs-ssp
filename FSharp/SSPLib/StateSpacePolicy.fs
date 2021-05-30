namespace SSPLib

module StateSpacePolicy =
    type situationsInState = Situations of seq<StateSpace.situation> | Not_reachable
    type coursesBetweenSituations = Courses of seq<State.transFun> | No_transitions
    type systemSituationMatrix = situationsInState array
    type systemTransformationMatrix = coursesBetweenSituations SquareMatrix.matrix

    let convolute situations courses = 
        match situations,courses with
        | Not_reachable,No_transitions -> Not_reachable
        | Situations _, No_transitions -> Not_reachable
        | Not_reachable, Courses _ -> Not_reachable
        | Situations sits, Courses trans_funs -> 
            let result = Seq.collect
                            (
                                fun sit -> 
                                    let new_situations = Seq.choose 
                                                            (fun trans_fun -> StateSpace.advanceSituation sit trans_fun |> StateSpace.filteringFun) 
                                                            trans_funs
                                    new_situations
                            )
                            sits
            Situations result
    let private _mergeSituationsInState sits1 sits2 = 
        match sits1, sits2 with
        | Not_reachable, Not_reachable -> Not_reachable
        | Situations s1, Not_reachable -> Situations s1
        | Not_reachable, Situations s2 -> Situations s2
        | Situations s1, Situations s2 -> Situations (Seq.append s1 s2)
    let private _limitSituationsInState transformer maxItems allSituationsInState =
        match allSituationsInState with
        | Not_reachable -> Not_reachable
        | Situations sits ->
                            (*let generatorInitState = filter sits
                            Situations (Seq.unfold 
                                                (fun (subseq,n) -> 
                                                    if not (Seq.isEmpty subseq) && n<maxItems then
                                                        let toReturn = Seq.head subseq
                                                        let tail = Seq.tail subseq
                                                        Some (toReturn,(tail,n+1))
                                                    else
                                                        None
                                                )
                                                (generatorInitState,0)
                                        )
                            *)
                            let transformedSits = transformer sits
                            Situations (Seq.truncate maxItems transformedSits)
    let private _multiply isLimited filterFun limitSize situationsMX transMX =
        Array.Parallel.mapi 
            (
                fun toState_id _ ->
                    let column_of_functions_to_state = SquareMatrix.column transMX toState_id
                    let newSituationsInState_toFlatten =
                        Array.mapi
                            (
                                fun fromState_id situationsInState ->
                                    let transitionsFromState_id = (Array.get column_of_functions_to_state fromState_id)
                                    convolute
                                        situationsInState
                                        transitionsFromState_id
                            )
                            situationsMX
                    let new_situations_in_state = Array.fold
                                                    (fun res_situation sits_to_merge -> _mergeSituationsInState res_situation sits_to_merge ) 
                                                    Not_reachable 
                                                    newSituationsInState_toFlatten
                    if isLimited then
                        new_situations_in_state |> _limitSituationsInState filterFun limitSize
                    else
                        new_situations_in_state
            )
            (Array.create transMX.length [])
    let multiply (situationsMX:systemSituationMatrix) transMX =
        //let config = {limit=false;filter=(fun x -> x)}
        _multiply false (fun x -> x) -1 situationsMX transMX 
    let multiplyLimited (situationsMX:systemSituationMatrix) transMX filterFun resultSize = 
        //let config = {limit=true;filter=filterFun}
        _multiply true filterFun resultSize situationsMX transMX
    let initSituationInState (sit:StateSpace.situation) =
        let res =  Seq.singleton sit
        (Situations res)
    let initSituationMatrix sis initStateIdx numOfStates =
        let result = Array.create numOfStates Not_reachable
        Array.set result initStateIdx sis;
        result
