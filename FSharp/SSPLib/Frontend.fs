namespace SSPLib

module Frontend =
    type computationStrategy = ComputeAll | ComputeLimited of int
    type destinationStrategy = FirstFound | Random
    type resultStrategy = All | First | Bests of Filter.filter | BestsAvailable of (Filter.metric*int)
    let chooseDestinationStateIdx strategy destinationStates = 
      match strategy with 
      | FirstFound -> (Seq.head destinationStates).state_idx
      | Random ->
        let rand = System.Random ();
        let sequence_idx = rand.Next(Seq.length destinationStates)
        (Seq.item sequence_idx destinationStates).state_idx
    let makeSystemTransformationMatrix trans_funs =
      let array_matrix_of_trans_funs = Array.init 
                                        (State.numberOfStates ()) 
                                        (
                                          fun _ -> 
                                              Array.init 
                                                (State.numberOfStates ()) 
                                                (fun _ -> [])
                                        )
      Seq.iter 
        (
          fun trans_fun -> 
            let row = trans_fun.fromIdx
            let column = trans_fun.toIdx 
            let current_element_array = Array.get array_matrix_of_trans_funs row in
            let current_element = Array.get current_element_array column in
            let new_element = (State.parseTransFun trans_fun)::current_element in
            Array.set current_element_array column (new_element);
            Array.set array_matrix_of_trans_funs row current_element_array
        )
        trans_funs 
      Array.map 
        (
          fun row -> 
            Array.map 
              (
                fun lotf -> 
                  if List.isEmpty lotf then 
                    StateSpacePolicy.No_transitions 
                  else 
                    StateSpacePolicy.Courses (List.toSeq lotf) 
              ) 
              row 
        )
        array_matrix_of_trans_funs |> SquareMatrix.make
    let makeSystem raw_trans_funs state state_idx num_of_states =
      let transition_matrix = makeSystemTransformationMatrix raw_trans_funs
      let init_situation = StateSpacePolicy.initSituationInState state
      let situation_matrix = StateSpacePolicy.initSituationMatrix init_situation state_idx num_of_states in
        situation_matrix,transition_matrix
    let isStateReached sits_matrix state_idx = 
      match Array.get sits_matrix state_idx with
      | StateSpacePolicy.Not_reachable -> false
      | StateSpacePolicy.Situations s_seq -> not (Seq.isEmpty s_seq)
    let private _fix_situations_in_state_to_not_reachable sits_matrix state_idx =
      Array.set sits_matrix state_idx StateSpacePolicy.Not_reachable
    let private _generic_search_for_situation_in_state 
      isLimited
      limitSize 
      sits_matrix 
      trans_matrix 
      state_idx 
      max_num_of_steps =
        let result = ref sits_matrix
        let is_reached = ref false
        let iter = ref 0 
        while ( (not !is_reached) && if max_num_of_steps <> -1 then !iter < max_num_of_steps else true ) do
          //printfn "Multiplying iteration: %d" !iter
          if not isLimited then
            result := StateSpacePolicy.multiply !result trans_matrix
          else
            result := StateSpacePolicy.multiplyLimited !result trans_matrix limitSize
          is_reached := isStateReached !result state_idx;
          if not !is_reached then
            _fix_situations_in_state_to_not_reachable !result state_idx;
          iter := !iter + 1
        done;
        !result,!iter,!is_reached
    let searchForSituationInState sits_matrix trans_matrix state_idx max_num_of_steps = 
      _generic_search_for_situation_in_state false -1 sits_matrix trans_matrix state_idx max_num_of_steps
    let searchForSituationInStateLimited sits_matrix trans_matrix state_idx max_num_of_steps limit =
      _generic_search_for_situation_in_state true limit sits_matrix trans_matrix state_idx max_num_of_steps
    let exportWalk (walk:StateSpace.walk) all_raw_trans_funs =
      let hashed_trans_funs = Seq.fold (fun htf tf -> Map.add tf.transitionIdx tf htf ) Map.empty all_raw_trans_funs;
      List.map (fun (we:State.transFun) -> Map.find we.transition_idx hashed_trans_funs ) walk |> List.rev
    let getWalkFromSituationMatrix strategy sm stateIdx =
      let situations_in_state = Array.get sm stateIdx
      match situations_in_state with
          | StateSpacePolicy.Not_reachable -> raise (invalidArg "state_idx" "Desired state is not reachable")
          | StateSpacePolicy.Situations s_seq -> 
            match strategy with 
            | First -> Seq.init 1 (fun _ -> (Seq.head s_seq).currentWalk)
            | All -> Seq.map (fun (s:StateSpace.situation) -> s.currentWalk ) s_seq 
            | Bests f -> Seq.filter f s_seq |> Seq.map (fun s -> s.currentWalk)
            | BestsAvailable (m,n) -> 
              let sorted = Seq.sortByDescending m s_seq
              Seq.truncate n sorted |> Seq.map (fun s -> s.currentWalk)
    let private _generic_search_for_walks_leading_to_state 
      isLimited
      limit
      sits_matrix 
      trans_matrix 
      state_idx 
      max_num_of_steps 
      strategy
      forceNoIdling = 
        let steps_left = ref max_num_of_steps
        let current_sits_matrix = ref sits_matrix
        let result = ref Seq.empty
        let is_found_at_least_once = ref false
        while !steps_left > 0 do
          let situations_matrix,num_of_steps_used,is_found = 
            if not isLimited then
              searchForSituationInState !current_sits_matrix trans_matrix state_idx !steps_left
            else
              searchForSituationInStateLimited !current_sits_matrix trans_matrix state_idx !steps_left limit
          if is_found then (
            printfn "Found a walk to a destination state in %d step(s)" num_of_steps_used
            is_found_at_least_once := true;
            result := Seq.append (getWalkFromSituationMatrix strategy situations_matrix state_idx) !result
            if forceNoIdling then
              Array.set situations_matrix state_idx StateSpacePolicy.Not_reachable
          )
          current_sits_matrix := situations_matrix
          steps_left := !steps_left - num_of_steps_used
          printfn "Remaining steps left:%d" !steps_left
        done;
        !result,!is_found_at_least_once
    let searchForWalksLeadingToState sits_matrix trans_matrix state_idx max_num_of_steps strategy noIdling = 
      _generic_search_for_walks_leading_to_state false -1 sits_matrix trans_matrix state_idx max_num_of_steps strategy noIdling
    let searchForWalksLeadingToStateLimited sits_matrix trans_matrix state_idx max_num_of_steps strategy limit noIdling =
      _generic_search_for_walks_leading_to_state true limit sits_matrix trans_matrix state_idx max_num_of_steps strategy noIdling