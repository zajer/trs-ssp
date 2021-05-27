namespace SSPLib

open FSharp.Data
open System.Text.RegularExpressions
module Data =
    let private _extractNumbers str =
        let number_regex_str = "[0-9]+"
        let number_regex = Regex(number_regex_str)
        let num1_match = number_regex.Match(str)
        let num2_match = num1_match.NextMatch()
        int num1_match.Value,int num2_match.Value
    let private _extractPairsOfNumbers str =
        let pairs_regex_str = "[0-9]+,[0-9]+"
        let pairs_regex = Regex(pairs_regex_str)
        let pairs = pairs_regex.Matches(str) |> Seq.map (fun m -> _extractNumbers m.Value)
        List.ofSeq pairs
    let private _parseTransFunData tfd_str =
      _extractPairsOfNumbers tfd_str
    [<Literal>]
    let private _TRANS_FUN_DATA_HEADER = "permutation with time shift"
    [<Literal>]
    let private _TRANS_FUN_REACT_HEADER = "react"
    [<Literal>]
    let private _TRANS_FROM_STATE_HEADER = "from"
    [<Literal>]
    let private _TRANS_TO_STATE_HEADER = "to"
    [<Literal>]
    let private _TRANS_FUN_CORRESPONDING_TRANSITION_HEADER = "corresponds to transition"
    [<Literal>]
    let private _CSV_HEADER = _TRANS_FUN_DATA_HEADER+","+_TRANS_FUN_REACT_HEADER+","+_TRANS_FROM_STATE_HEADER+","+_TRANS_TO_STATE_HEADER+","+_TRANS_FUN_CORRESPONDING_TRANSITION_HEADER
    [<Literal>]
    let private _CSV_SCHEMA = _TRANS_FUN_DATA_HEADER+"=string,"+_TRANS_FUN_REACT_HEADER+"=string,"+_TRANS_FROM_STATE_HEADER+"=int,"+_TRANS_TO_STATE_HEADER+"=int,"+_TRANS_FUN_CORRESPONDING_TRANSITION_HEADER+"=int"
    [<Literal>]
    let private _SAMPLE_CSV = _CSV_HEADER+"\n\"(3,5);(1,7)\",react1,1,2,3\n\"(7,3);(2,4)\",react2,1,2,3"
    type TransFunCsvType =  CsvProvider<Schema = _CSV_SCHEMA, Sample = _SAMPLE_CSV, HasHeaders=true>
    let importTransFuns (filename:string) = 
      let imported_trans_funs_sll = CsvFile.Load (uri = filename, hasHeaders = true)
      imported_trans_funs_sll.Rows |> Seq.map 
        (
          fun r ->
            {
                permutationWithTimeShift = _parseTransFunData r.[_TRANS_FUN_DATA_HEADER];
                reactLabel = r.[_TRANS_FUN_REACT_HEADER];
                fromIdx = r.[_TRANS_FROM_STATE_HEADER] |> int ;
                toIdx = r.[_TRANS_TO_STATE_HEADER] |> int ;
                transitionIdx = r.[_TRANS_FUN_CORRESPONDING_TRANSITION_HEADER] |> int
            }
        )
    let private _transFunDataToString tfd =
      List.map (fun (aid,ts) -> "("+aid.ToString()+","+ts.ToString()+")") tfd |> String.concat ";"
    let exportTransFuns tfs filename =
      let tfs_csv = List.map 
                        (
                          fun (tf) ->
                            TransFunCsvType.Row(
                                _transFunDataToString tf.permutationWithTimeShift, 
                                tf.reactLabel, 
                                tf.fromIdx, 
                                tf.toIdx, 
                                tf.transitionIdx
                            ) 
                        ) 
                        tfs
      let csv = new TransFunCsvType(tfs_csv)
      csv.Save(path=filename)
    let importDestStates filename =
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
    