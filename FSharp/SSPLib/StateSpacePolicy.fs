namespace SSPLib

module StateSpacePolicy =
    type situationsInState = Situations of StateSpace.situation list | NotReachable
    type coursesBetweenSituations = Courses of State.transFun list | NoTransitions
    type systemSituationMatrix = situationsInState array
    type systemTransformationMatrix = coursesBetweenSituations SquareMatrix.matrix

    let convolute situations courses = 
        match situations,courses with
        | NotReachable,NoTransitions -> []
        | Situations _, NoTransitions -> []
        | NotReachable, Courses _ -> []
        | Situations sits, Courses trans_funs -> 
            List.collect
                (
                    fun sit -> 
                        let new_situations = List.choose 
                                                (fun trans_fun -> StateSpace.advanceSituation sit trans_fun |> StateSpace.filteringFun) 
                                                trans_funs
                        new_situations
                )
                sits
    (*let private _mergeSituationsInState sits1 sits2 = 
        match sits1, sits2 with
        | NotReachable, NotReachable -> NotReachable
        | Situations s1, NotReachable -> Situations s1
        | NotReachable, Situations s2 -> Situations s2
        | Situations s1, Situations s2 -> Situations (List.append s1 s2)*)
    let private _limitSituationsInState transformer maxItems allSituationsInState =
        let transformedSits = transformer allSituationsInState
        List.truncate maxItems transformedSits
    let private _multiply isLimited filterFun limitSize situationsMX transMX =
        Array.Parallel.mapi 
            (
                fun targetState _ ->
                    let columnOfTransitionFunctionsToTargetState = SquareMatrix.column transMX targetState

                    let allPartialsResultsSeparate =
                        Array.Parallel.mapi
                            (
                                fun sourceState currentSituationsInState ->
                                    let transitionsFromSourceState = (Array.get columnOfTransitionFunctionsToTargetState sourceState)
                                    let partialResult =
                                            convolute
                                                currentSituationsInState
                                                transitionsFromSourceState
                                    partialResult
                            )
                            situationsMX
                    (*let new_situations_in_state = Array.fold
                                                    (fun res_situation sits_to_merge -> _mergeSituationsInState res_situation sits_to_merge ) 
                                                    NotReachable 
                                                    newSituationsInState_toFlatten*)
                    let allPartialsResults = List.concat allPartialsResultsSeparate
                    let resultRaw = 
                        if isLimited then
                            _limitSituationsInState filterFun limitSize allPartialsResults
                        else
                            allPartialsResults
                    let result =
                        if List.isEmpty resultRaw then
                            NotReachable
                        else
                            Situations resultRaw
                    result
            )
            (Array.create transMX.length [])
    let multiply (situationsMX:systemSituationMatrix) transMX =
        //let config = {limit=false;filter=(fun x -> x)}
        _multiply false (fun x -> x) -1 situationsMX transMX 
    let multiplyLimited (situationsMX:systemSituationMatrix) transMX filterFun resultSize = 
        //let config = {limit=true;filter=filterFun}
        _multiply true filterFun resultSize situationsMX transMX
    let initSituationInState (sit:StateSpace.situation) =
        let res =  [sit]
        (Situations res)
    let initSituationMatrix sis initStateIdx numOfStates =
        let result = Array.create numOfStates NotReachable
        Array.set result initStateIdx sis;
        result
