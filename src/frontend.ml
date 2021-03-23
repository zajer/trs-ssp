module IntSet = Set.Make(Int)
let _extract_next_number pair_of_numbers_str start_position =
  let number_regex = Str.regexp "[0-9]+" in
  let _ = Str.search_forward number_regex pair_of_numbers_str start_position 
  and number =  Str.matched_string pair_of_numbers_str |> int_of_string
  and new_start_pos = Str.match_end () in
  number,new_start_pos
let _extract_next_pair_of_numbers tfd_str start_position =
  let pair_of_numbers_regex = Str.regexp "[0-9]+,[0-9]+" in
  let _ = Str.search_forward pair_of_numbers_regex tfd_str start_position in
  let matched = Str.matched_string tfd_str 
  and result_new_start_pos = Str.match_end () in
  let number1,new_start_pos = _extract_next_number matched 0 in
  let number2,_ = _extract_next_number matched new_start_pos  in
  number1,number2,result_new_start_pos
let _parse_trans_fun_data tfd_str =
  let result = ref [] 
  and parse_pointer = ref 0 
  and end_flag = ref false
  in
    while not !end_flag do
    (
      try 
        let num1,num2,pointer_tmp = _extract_next_pair_of_numbers tfd_str !parse_pointer in
        parse_pointer := pointer_tmp;
        result := (num1,num2)::!result;   
      with Not_found -> end_flag := true
    )
    done;
    !result
let _string_list_2_5tuple sl = 
  assert (List.length sl = 5);
  List.nth sl 0,List.nth sl 1,List.nth sl 2,List.nth sl 3,List.nth sl 4
let import_trans_funs filename = 
  let imported_trans_funs_sll = Csv.load filename |> List.tl in
  List.map 
    (
      fun sl ->
        let tf_data_str,react_label,from_idx_str,to_idx_str,correspondence_numb_str = _string_list_2_5tuple sl in
            let tf_data = _parse_trans_fun_data tf_data_str |> List.rev in
            {
                State.permutation_with_time_shift=tf_data;
                react_label;from_idx=from_idx_str|>int_of_string;
                to_idx=to_idx_str|>int_of_string;
                transition_idx=correspondence_numb_str|>int_of_string
            }
    )
    imported_trans_funs_sll
let _TRANS_FUN_DATA_HEADER = "permutation with time shift"
let _TRANS_FUN_REACT_HEADER = "react"
let _TRANS_FROM_STATE_HEADER = "from"
let _TRANS_TO_STATE_HEADER = "to"
let _TRANS_FUN_CORRESPONDING_TRANSITION = "corresponds to transition"
let _trans_fun_data_2_string tfd =
  List.map (fun (aid,ts) -> "("^(string_of_int aid)^","^(string_of_int ts)^")") tfd |> String.concat ";"
let export_trans_funs tfs filename =
  let tfs_csv = List.map 
    (
      fun (tf) -> 
        [(_trans_fun_data_2_string tf.State.permutation_with_time_shift);(tf.react_label);(string_of_int tf.from_idx); (string_of_int tf.to_idx);(string_of_int (tf.transition_idx))]
    ) 
    tfs
  and header = [_TRANS_FUN_DATA_HEADER;_TRANS_FUN_REACT_HEADER; _TRANS_FROM_STATE_HEADER;_TRANS_TO_STATE_HEADER; _TRANS_FUN_CORRESPONDING_TRANSITION] in
  Csv.save filename (header::tfs_csv)
let _string_list_2_2tuple sl = 
  assert (List.length sl = 2);
  List.nth sl 0,List.nth sl 1
let rec _find_inner_elts str start_position accu = 
  let inner_elt_regex = Str.regexp "[a-zA-Z0-9 ]+" in
  try
    let _ = Str.search_forward inner_elt_regex str start_position
    and matched = Str.matched_string str
    and new_start_pos = Str.match_end () in
      _find_inner_elts str new_start_pos (matched::accu)
  with Not_found -> accu
let _parse_list_of_string str = 
  let inner_elts = _find_inner_elts str 0 [] in
  inner_elts
let import_dest_states file_name =
  let dest_states_csv = Csv.load file_name in
  let dest_states =
    List.map 
      (
        fun sl -> let idx_str,string_list_str = _string_list_2_2tuple sl in
        {State.state_idx=(int_of_string idx_str); patts_found= _parse_list_of_string string_list_str}
      )
      dest_states_csv
  in
    dest_states
type destination_strategy = FirstFound | Random
let destination_state_idx strategy destination_states = 
    match strategy with 
    | FirstFound -> (List.hd destination_states).State.state_idx
    | Random ->
      Random.self_init ();
      let sequence_idx = Random.int (List.length destination_states) in
      (List.nth destination_states sequence_idx).state_idx

module Make ( S : State.S ) = struct
  module SS = State_space.Make(S)
  module SSP = State_space_policy.Make(SS)

  type state = S.t
  type state_trans_fun = S.trans_fun
  type walk = state_trans_fun list
  type situation =  SS.situation
  type situations_in_state = SSP.situations_in_state
  type courses_between_situations = SSP.courses_between_situations
  type system_situation_matrix = SSP.system_situation_matrix
  type system_transformation_matrix = SSP.system_transformation_matrix

  let make_system_transformation_matrix trans_funs =
    let array_matrix_of_trans_funs = Array.make (S.num_of_states ()) (Array.make (S.num_of_states ()) []) in
    List.iter 
      (
        fun trans_fun -> 
          let row = trans_fun.State.from_idx
          and column = trans_fun.State.to_idx in
          let current_element_array = Array.get array_matrix_of_trans_funs row in
          let current_element = Array.get current_element_array column in
          let new_element = (S.parse_trans_fun trans_fun)::current_element in
          Array.set current_element_array column (new_element);
          Array.set array_matrix_of_trans_funs row current_element_array
      )
      trans_funs 
    ;
    Array.map 
      (fun row -> 
        Array.map 
          (fun lotf -> if List.compare_length_with lotf 0 > 0 then SSP.Courses (List.to_seq lotf) else No_transitions ) 
          row 
      )
      array_matrix_of_trans_funs |> Square_matrix.make
  
  let make_ssp_system filename state ~state_idx ~num_of_states =
    let raw_trans_funs = import_trans_funs filename in
    let transition_matrix = make_system_transformation_matrix raw_trans_funs
    and init_situation = SSP.init_situation_in_state state in
    let situation_matrix = SSP.init_situation_matrix init_situation ~state_idx  ~num_of_states in
      situation_matrix,transition_matrix
  let is_state_reached sits_matrix state_idx = 
    match Array.get sits_matrix state_idx with
    | SSP.Not_reachable -> false
    | SSP.Situations s_seq -> List.length (List.of_seq s_seq) > 0
  let _fix_situations_in_state_to_not_reachable sits_matrix state_idx =
    Array.set sits_matrix state_idx SSP.Not_reachable
  let search_for_situation_in_state sits_matrix trans_matrix ~state_idx ~max_num_of_steps = 
    let filtering_fun = fun sit -> if S.is_negligible sit.SS.current_state then None else Some sit in
    let result = ref sits_matrix in
    let is_reached = ref (is_state_reached !result state_idx) 
    and iter = ref 0 in
      while not (!is_reached) && if max_num_of_steps <> -1 then !iter < max_num_of_steps else true do
        result := SSP.multiply filtering_fun sits_matrix trans_matrix;
        is_reached := is_state_reached !result state_idx;
        if not !is_reached then
          _fix_situations_in_state_to_not_reachable !result state_idx;
        iter := !iter +1
      done;
      !result,!iter,!is_reached
  let export_walk walk all_raw_trans_funs=
    let ids = List.fold_left (fun set e -> IntSet.add e.S.transition_idx set ) IntSet.empty walk in
    let filtered_raw_trans_funs = List.filter (fun tf -> IntSet.mem tf.State.transition_idx ids ) all_raw_trans_funs in
    filtered_raw_trans_funs
end

