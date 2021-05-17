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
