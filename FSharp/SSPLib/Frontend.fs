namespace SSPLib

open FSharp.Data
open System.Text.RegularExpressions

module Frontend =
    let _extract_numbers str =
        let number_regex_str = "[0-9]+"
        let number_regex = Regex(number_regex_str)
        let num1_match = number_regex.Match(str)
        let num2_match = num1_match.NextMatch()
        int num1_match.Value,int num2_match.Value
    let _extract_pairs_of_numbers str =
        let pairs_regex_str = "[0-9]+,[0-9]+"
        let pairs_regex = Regex(pairs_regex_str)
        let pairs = pairs_regex.Matches(str) |> Seq.map (fun m -> _extract_numbers m.Value)
        List.ofSeq pairs
    let _parse_trans_fun_data tfd_str =
      _extract_pairs_of_numbers tfd_str
    [<Literal>]
    let _TRANS_FUN_DATA_HEADER = "permutation with time shift"
    [<Literal>]
    let _TRANS_FUN_REACT_HEADER = "react"
    [<Literal>]
    let _TRANS_FROM_STATE_HEADER = "from"
    [<Literal>]
    let _TRANS_TO_STATE_HEADER = "to"
    [<Literal>]
    let _TRANS_FUN_CORRESPONDING_TRANSITION = "corresponds to transition"
    [<Literal>]
    let _CSV_HEADER = _TRANS_FUN_DATA_HEADER+","+_TRANS_FUN_REACT_HEADER+","+_TRANS_FROM_STATE_HEADER+","+_TRANS_TO_STATE_HEADER+","+_TRANS_FUN_CORRESPONDING_TRANSITION
    [<Literal>]
    let _CSV_SCHEMA = _TRANS_FUN_DATA_HEADER+"=string,"+_TRANS_FUN_REACT_HEADER+"=string,"+_TRANS_FROM_STATE_HEADER+"=int,"+_TRANS_TO_STATE_HEADER+"=int,"+_TRANS_FUN_CORRESPONDING_TRANSITION+"=int"
    [<Literal>]
    let private _sample_csv = _CSV_HEADER+"\n\"(3,5);(1,7)\",react1,1,2,3\n\"(7,3);(2,4)\",react2,1,2,3"
    type TransFunCsvType =  CsvProvider<Schema = _CSV_SCHEMA, Sample = _sample_csv, HasHeaders=true>
    let import_trans_funs (filename:string) = 
      let imported_trans_funs_sll = CsvFile.Load (uri = filename, hasHeaders = true)
      imported_trans_funs_sll.Rows |> Seq.map 
        (
          fun r ->
            {
                permutation_with_time_shift = _parse_trans_fun_data r.[_TRANS_FUN_DATA_HEADER];
                react_label = r.[_TRANS_FUN_REACT_HEADER];
                from_idx = r.[_TRANS_FROM_STATE_HEADER] |> int ;
                to_idx = r.[_TRANS_TO_STATE_HEADER] |> int ;
                transition_idx = r.[_TRANS_FUN_CORRESPONDING_TRANSITION] |> int
            }
        )
    let _trans_fun_data_2_string tfd =
      List.map (fun (aid,ts) -> "("+aid.ToString()+","+ts.ToString()+")") tfd |> String.concat ";"
    let export_trans_funs tfs filename =
      let tfs_csv = List.map 
                        (
                          fun (tf) ->
                            TransFunCsvType.Row(
                                _trans_fun_data_2_string tf.permutation_with_time_shift, 
                                tf.react_label, 
                                tf.from_idx, 
                                tf.to_idx, 
                                tf.transition_idx
                            ) 
                        ) 
                        tfs
      let csv = new TransFunCsvType(tfs_csv)
      csv.Save(path=filename)
    let import_dest_states filename =
        let dest_states_csv = CsvFile.Load  (uri=filename,hasHeaders=false)
        let dest_states = dest_states_csv.Rows |> Seq.map 
                                                      (
                                                          //let idx_str,string_list_str = _string_list_2_2tuple sl in
                                                          fun r -> 
                                                            {
                                                                state_idx = r.Item 0 |> int;
                                                                patts_found = r.Item 1
                                                            }
                                                      )
                                                      
        dest_states
    type destination_strategy = FirstFound | Random
    let destination_state_idx strategy destination_states = 
      match strategy with 
      | FirstFound -> (Seq.head destination_states).state_idx
      | Random ->
        let rand = System.Random ();
        let sequence_idx = rand.Next(Seq.length destination_states)
        (Seq.item sequence_idx destination_states).state_idx
    let make_system_transformation_matrix trans_funs =
      let array_matrix_of_trans_funs = Array.init 
                                        (S.num_of_states ()) 
                                        (
                                          fun _ -> 
                                              Array.init 
                                                (S.num_of_states ()) 
                                                (fun _ -> [])
                                        )
      List.iter 
        (
          fun trans_fun -> 
            let row = trans_fun.from_idx
            let column = trans_fun.to_idx 
            let current_element_array = Array.get array_matrix_of_trans_funs row in
            let current_element = Array.get current_element_array column in
            let new_element = (S.parse_trans_fun trans_fun)::current_element in
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
                    SSP.No_transitions 
                  else 
                    SSP.Courses (List.toSeq lotf) 
              ) 
              row 
        )
        array_matrix_of_trans_funs |> SquareMatrix.make
    let make_ssp_system raw_trans_funs state state_idx num_of_states =
      let transition_matrix = make_system_transformation_matrix (List.ofSeq raw_trans_funs)
      let init_situation = SSP.init_situation_in_state state
      let situation_matrix = SSP.init_situation_matrix init_situation state_idx num_of_states in
        situation_matrix,transition_matrix
    let is_state_reached sits_matrix state_idx = 
      match Array.get sits_matrix state_idx with
      | SSP.Not_reachable -> false
      | SSP.Situations s_seq -> not (List.isEmpty (List.ofSeq s_seq))
    let _fix_situations_in_state_to_not_reachable sits_matrix state_idx =
      Array.set sits_matrix state_idx SSP.Not_reachable
    let search_for_situation_in_state sits_matrix trans_matrix state_idx max_num_of_steps = 
      let (filtering_fun:SS.situation -> SS.situation option) = fun sit -> if S.is_negligible sit.current_state then None else Some sit
      let result = ref sits_matrix
      let is_reached = ref (is_state_reached !result state_idx) 
      let iter = ref 0 
      while not ( (!is_reached) && if max_num_of_steps <> -1 then !iter < max_num_of_steps else true ) do
          result := SSP.multiply filtering_fun !result trans_matrix;
          is_reached := is_state_reached !result state_idx;
          if not !is_reached then
            _fix_situations_in_state_to_not_reachable !result state_idx;
          iter := !iter + 1
      done;
      !result,!iter,!is_reached
    let export_walk (walk:SS.walk) all_raw_trans_funs =
      let hashed_trans_funs = Seq.fold (fun htf tf -> Map.add tf.transition_idx tf htf ) Map.empty all_raw_trans_funs;
      List.map (fun (we:S.trans_fun) -> Map.find we.transition_idx hashed_trans_funs ) walk |> List.rev
    let walk_from_situation_matrix strategy sm state_idx =
      match strategy with 
      | FirstFound -> 
        (
          let situations_in_state = Array.get sm state_idx
          match situations_in_state with
          | SSP.Not_reachable -> raise (invalidArg "state_idx" "Desired state is not reachable")
          | SSP.Situations s_seq -> 
            let situation = List.ofSeq s_seq |> List.head
            situation.current_walk
        )
      | Random ->
        (
          let rand = System.Random ()
          let situations_in_state = Array.get sm state_idx
          match situations_in_state with
          | SSP.Not_reachable -> raise (invalidArg "state_idx" "Desired state is unreachable")
          | SSP.Situations s_seq -> 
          let situations = List.ofSeq s_seq
          let situation_idx = rand.Next (List.length situations)
          let situation = List.item situation_idx situations
          situation.current_walk
        )