open OUnit2
open Policy
let test_import_trans_funs_1 _ =
    let filename = "trans_funs.csv" in
    let expected_results = [
        {State.permutation_with_time_shift=[(1,2);(3,2);(2,0);(4,0)];react_label="r1";from_idx=0;to_idx=1;transition_idx=1};
        {permutation_with_time_shift=[(1,0);(2,0);(3,0);(4,1)];react_label="r3";from_idx=1;to_idx=2;transition_idx=3};
        {permutation_with_time_shift=[(4,0);(3,3);(2,3);(1,0)];react_label="r2";from_idx=2;to_idx=0;transition_idx=2}
    ]
    and results = Frontend.import_trans_funs filename in
    assert_equal
        ~msg:"Imported transition functions are not equal to expected"
        expected_results results
let test_export_trans_funs_1 ctx = 
    let filename = "exported_trans_funs.csv" 
    and tmp_dir = bracket_tmpdir ~prefix:"tmp_" ctx in
    let exported_trans_funs = [
        {State.permutation_with_time_shift=[(1,2);(3,2);(2,0);(4,0)];react_label="r1";from_idx=0;to_idx=1;transition_idx=1};
        {permutation_with_time_shift=[(1,0);(2,0);(3,0);(4,1)];react_label="r3";from_idx=1;to_idx=2;transition_idx=3};
        {permutation_with_time_shift=[(4,0);(3,3);(2,3);(1,0)];react_label="r2";from_idx=2;to_idx=0;transition_idx=2}
    ] in
    Frontend.export_trans_funs exported_trans_funs (tmp_dir^filename);
    let imported_trans_funs = Frontend.import_trans_funs (tmp_dir^filename) in
    assert_equal 
        ~msg:"Imported trans funs are not equal to exported"
        exported_trans_funs
        imported_trans_funs
let _destination_state_2_string ds =
  let state_idx = string_of_int ds.State.state_idx
          and descriptions = "["^(String.concat ";" ds.patts_found)^"]"
          in
          "{"^state_idx^" ;"^descriptions^"}"
let _are_dest_states_equal ds1 ds2 =
  let res = ds1.State.state_idx = ds2.State.state_idx &&
  List.for_all (fun ds1e -> List.exists (fun ds2e -> ds1e = ds2e) ds2.patts_found) ds1.patts_found in
  res
let test_import_dest_states_1 _ =
  let expected_dest_state_1 = { State.state_idx=7;patts_found=["yolo";"swag"] }
  and expected_dest_state_2 = { State.state_idx=21;patts_found=["bilbo";"baggins"] }
  and source_file = "dest_states.csv"
  in
  let imported_dest_states = Frontend.import_dest_states source_file in
  assert_equal 
    ~msg:"There should be exactly two imported destination states"
    2
    (List.length imported_dest_states);
  assert_equal
    ~msg:"Imported destinations states do not match with the expected"
    ~cmp:
      (
        fun dsl1 dsl2 -> 
          List.for_all (fun dsl1e -> List.exists (fun dsl2e -> _are_dest_states_equal dsl1e dsl2e) dsl2) dsl1
      )
    ~printer:
      (
        fun dsl ->
          let dsl_sl = List.map (fun ds -> _destination_state_2_string ds) dsl in
          String.concat ";" dsl_sl
      )
      [expected_dest_state_1;expected_dest_state_2]
      imported_dest_states
module DF = Frontend.Make(DummyState2A_Parsable)

let _compare_transformation_matrices m1 m2 =
    if m1.Square_matrix.length <> m2.Square_matrix.length then
        false
    else
        Array.for_all2 
            (
                fun row_m1 row_m2 -> 
                    Array.for_all2 
                        (
                            fun elt_in_row_m1 elt_in_row_m2 -> 
                                match elt_in_row_m1,elt_in_row_m2 with
                                | DF.SSP.No_transitions,DF.SSP.No_transitions -> true
                                | No_transitions, Courses _ -> false
                                | Courses _, No_transitions -> false
                                | Courses c1_seq,Courses c2_seq -> 
                                    let c1_l = List.of_seq c1_seq
                                    and c2_l = List.of_seq c2_seq in
                                    List.for_all2 (fun tf1 tf2 ->  tf1.DummyState2A_Parsable.transition_idx = tf2.DummyState2A_Parsable.transition_idx ) c1_l c2_l
                        )
                        row_m1 row_m2
            )
            m1.elements
            m2.elements
let _courses_between_sits_to_string c =
    let lot = List.of_seq c in
    List.map ( fun tf -> "tid:"^(string_of_int tf.DummyState2A_Parsable.transition_idx)) lot |> String.concat ";"
let test_make_transformation_matrix_1 _ =
    let matrix_input = [
        {State.permutation_with_time_shift=[(1,2);(2,0)];react_label="r1";from_idx=0;to_idx=1;transition_idx=1};
        {permutation_with_time_shift=[(2,1);(1,0)];react_label="r3";from_idx=1;to_idx=2;transition_idx=3};
        {permutation_with_time_shift=[(1,0);(2,3)];react_label="r2";from_idx=2;to_idx=0;transition_idx=2}
    ] 
    and state_trans_func_1 = {DummyState2A_Parsable.func=DummyState2A_Parsable.fun_1;transition_idx=1}
    and state_trans_func_2 = {DummyState2A_Parsable.func=DummyState2A_Parsable.fun_2;transition_idx=3} 
    and state_trans_func_3 = {DummyState2A_Parsable.func=DummyState2A_Parsable.fun_3;transition_idx=2} 
    in
    let expected_matrix = Square_matrix.init_single DF.SSP.No_transitions 3
    and result_matrix = DF.make_system_transformation_matrix matrix_input in
    Square_matrix.update expected_matrix ~row:0 ~column:1 (DF.SSP.Courses (Seq.return state_trans_func_1));
    Square_matrix.update expected_matrix ~row:1 ~column:2 (DF.SSP.Courses (Seq.return state_trans_func_2));
    Square_matrix.update expected_matrix ~row:2 ~column:0 (DF.SSP.Courses (Seq.return state_trans_func_3));
    assert_equal
        ~msg:"Result transformation matrix is not equal to expected"
        ~cmp:_compare_transformation_matrices
        ~printer:
            ( fun mx ->
                let str = Square_matrix.to_string (fun sotf -> match sotf with | DF.SSP.No_transitions -> "NTS" | Courses c -> _courses_between_sits_to_string c ) mx 
                in 
                    ("\n"^str)
            )
        expected_matrix
        result_matrix
let test_is_state_reached_1 _ = 
    let situation_matrix = Array.make 777 DF.SSP.Not_reachable in
    Array.set situation_matrix 7 (DF.SSP.Situations Seq.empty);
    assert_bool "State suppose to be unreachable because there are no situations" (not (DF.is_state_reached situation_matrix 7))
let test_is_state_reached_2 _ = 
    let situation_matrix = Array.make 777 DF.SSP.Not_reachable in
    Array.set situation_matrix 7 (DF.SSP.Situations (List.to_seq [ DF.SS.init_situation (A ((1,0),(2,0)) ) ] ) );
    assert_bool "State suppose to be reachable" (DF.is_state_reached situation_matrix 7)
let _compare_situations s1 s2 =
    s1.DF.SS.current_state = s2.DF.SS.current_state && 
    if List.compare_lengths s1.current_walk s2.current_walk = 0 then
        List.for_all2 (fun we1 we2 -> we1.DummyState2A_Parsable.transition_idx = we2.DummyState2A_Parsable.transition_idx) s1.current_walk s2.current_walk
    else
        false
let _compare_list_of_situations los1 los2 =
    List.for_all 
        (
            fun s1 -> 
                List.exists (fun s2 -> _compare_situations s1 s2) los2
        )
        los1
let _state_2_string s =
    match s with
    | DummyState2A_Parsable.A ((ai1,t1),(ai2,t2)) ->
    "("^(string_of_int ai1)^","^(string_of_int t1)^"),"^
    "("^(string_of_int ai2)^","^(string_of_int t2)^")"
    | Null -> "Null"
let _trans_func_2_string stf =
    string_of_int stf.DummyState2A_Parsable.transition_idx
let _walk_2_string w = 
    let result = List.map (fun stf -> _trans_func_2_string stf ) w |> String.concat "," in
    "["^result^"]"
let _situation_2_string s = 
    let state = s.DF.SS.current_state |> _state_2_string
    and walk = s.current_walk |> _walk_2_string in
    "{"^state^","^walk^"}"
let _list_of_situations_2_string = (fun los -> List.map (fun s-> _situation_2_string s) los |> String.concat "," )
let _situation_in_state_2_string sits_in_state state_idx =
    match sits_in_state with
    | DF.SSP.Not_reachable -> (string_of_int state_idx)^":Not reachable"
    | DF.SSP.Situations s -> (string_of_int state_idx)^":"^(List.of_seq s |> _list_of_situations_2_string)
let _situation_matrix_2_string sm =
    Array.mapi 
        (fun state_id sits_in_state -> _situation_in_state_2_string sits_in_state state_id ) 
        sm 
        |> Array.to_list |> String.concat " ; " 
let _compare_situations s1 s2 =
    s1.DF.SS.current_state = s2.DF.SS.current_state && 
    if List.compare_lengths s1.current_walk s2.current_walk = 0 then
        List.for_all2 (fun we1 we2 -> we1.DummyState2A_Parsable.transition_idx = we2.DummyState2A_Parsable.transition_idx) s1.current_walk s2.current_walk
    else
        false
let _compare_list_of_situations los1 los2 =
    List.for_all 
        (
            fun s1 -> 
                List.exists (fun s2 -> _compare_situations s1 s2) los2
        )
        los1
let _compare_array_of_situations_in_state losis1 losis2 =
    Array.for_all2
        (
            fun sis1 sis2 -> 
                match sis1,sis2 with
                | DF.SSP.Not_reachable,DF.SSP.Not_reachable -> true
                | Not_reachable , Situations _ -> false
                | Situations _ , Not_reachable -> false
                | Situations s1, Situations s2 -> _compare_list_of_situations (List.of_seq s1) (List.of_seq s2)
        )
        losis1
        losis2
let test_search_for_situation_in_state_1 _ =
    let init_state = DummyState2A_Parsable.A ((1,0),(2,0))
    and state_trans_func_1 = {DummyState2A_Parsable.func= DummyState2A_Parsable.fun_1;transition_idx=3} 
    and state_trans_func_2 = {DummyState2A_Parsable.func= DummyState2A_Parsable.fun_2;transition_idx=7} in
    let init_situation_in_state = DF.SSP.init_situation_in_state (DF.SS.init_situation init_state) 
    and courses_between_states_0_1 = DF.SSP.Courses (Seq.return state_trans_func_1)
    and courses_between_states_0_2 = DF.SSP.Courses (Seq.return state_trans_func_2) in
    let init_situation_matrix = DF.SSP.init_situation_matrix init_situation_in_state ~state_idx:0 ~num_of_states:3 in
    let trans_matrix = Policy.Square_matrix.init_single DF.SSP.No_transitions 3 in
    Square_matrix.update trans_matrix ~row:0 ~column:1 courses_between_states_0_1;
    Square_matrix.update trans_matrix ~row:0 ~column:2 courses_between_states_0_2;
    let result_sits_matrix,steps,is_reached = DF.search_for_situation_in_state init_situation_matrix trans_matrix ~state_idx:1 ~max_num_of_steps:777 
    and expected_sits_matrix = Array.make 3 DF.SSP.Not_reachable
    and expected_num_of_steps = 1
    and expected_reachability_flag = true
    in
    Array.set expected_sits_matrix 1 (DF.SSP.Situations (Seq.return { DF.SS.current_state=DummyState2A_Parsable.A ((1,2),(2,0));current_walk=[state_trans_func_1] } ));
    Array.set expected_sits_matrix 2 (DF.SSP.Situations (Seq.return { DF.SS.current_state=DummyState2A_Parsable.A ((2,1),(1,0));current_walk=[state_trans_func_2] } ));
    assert_equal
        ~msg:"State suppose to be reached" 
        expected_reachability_flag
        is_reached;
    assert_equal
        ~msg:"Result should be computed with one step"
        ~printer:(fun i -> string_of_int i)
        expected_num_of_steps
        steps;
    assert_equal
        ~msg:"Result situation matrix is not equal to expected"
        ~printer: _situation_matrix_2_string
        ~cmp:_compare_array_of_situations_in_state
        expected_sits_matrix
        result_sits_matrix
let test_search_for_situation_in_state_2 _ =
    let init_state = DummyState2A_Parsable.A ((1,0),(2,0))
    (*and state_trans_func_1 = {DummyState2A_Parsable.func= DummyState2A_Parsable.fun_1;transition_idx=3} *)
    and state_trans_func_2 = {DummyState2A_Parsable.func= DummyState2A_Parsable.fun_2;transition_idx=7} in
    let init_situation_in_state = DF.SSP.init_situation_in_state (DF.SS.init_situation init_state) 
    (*and courses_between_states_0_1 = DF.SSP.Courses (Seq.return state_trans_func_1)*)
    and courses_between_states_0_2 = DF.SSP.Courses (Seq.return state_trans_func_2) in
    let init_situation_matrix = DF.SSP.init_situation_matrix init_situation_in_state ~state_idx:0 ~num_of_states:3 in
    let trans_matrix = Policy.Square_matrix.init_single DF.SSP.No_transitions 3 
    and num_of_steps = 777 in
    Square_matrix.update trans_matrix ~row:0 ~column:2 courses_between_states_0_2;
    let _,steps,is_reached = DF.search_for_situation_in_state init_situation_matrix trans_matrix ~state_idx:1 ~max_num_of_steps:num_of_steps
    and expected_num_of_steps = num_of_steps
    and expected_reachability_flag = false
    in
    assert_equal
        ~msg:"State suppose to be unreachable" 
        expected_reachability_flag
        is_reached;
    assert_equal
        ~msg:("Result should be computed with "^(string_of_int num_of_steps)^" steps")
        ~printer:(fun i -> string_of_int i)
        expected_num_of_steps
        steps
let test_search_for_situation_in_state_3 _ =
    let init_state = DummyState2A_Parsable.A ((1,0),(2,0))
    and state_trans_func_1 = {DummyState2A_Parsable.func= DummyState2A_Parsable.fun_1;transition_idx=3} 
    and state_trans_func_2 = {DummyState2A_Parsable.func= DummyState2A_Parsable.fun_4;transition_idx=7} in
    let init_situation_in_state = DF.SSP.init_situation_in_state (DF.SS.init_situation init_state) 
    and courses_between_states_0_1 = DF.SSP.Courses (Seq.return state_trans_func_1)
    and courses_between_states_1_2 = DF.SSP.Courses (Seq.return state_trans_func_2) in
    let init_situation_matrix = DF.SSP.init_situation_matrix init_situation_in_state ~state_idx:0 ~num_of_states:3 in
    let trans_matrix = Policy.Square_matrix.init_single DF.SSP.No_transitions 3
    and num_of_steps = 777 in
    Square_matrix.update trans_matrix ~row:0 ~column:1 courses_between_states_0_1;
    Square_matrix.update trans_matrix ~row:1 ~column:2 courses_between_states_1_2;
    let result_situation_matrix,steps,is_reached = DF.search_for_situation_in_state init_situation_matrix trans_matrix ~state_idx:2 ~max_num_of_steps:num_of_steps 
    and expected_num_of_steps = num_of_steps
    and expected_reachability_flag = false in
    assert_equal
        ~msg:("Result should be computed in "^(string_of_int num_of_steps)^" steps")
        ~printer:(fun i -> string_of_int i)
        expected_num_of_steps
        steps;
    assert_equal
        ~msg:"State suppose to be unreachable" 
        expected_reachability_flag
        is_reached;
    assert_bool
        "No state should be marked as reachable in two steps"
        (Array.for_all (fun sis -> match sis with | DF.SSP.Not_reachable -> true | Situations _ -> false) result_situation_matrix )
let test_search_for_situation_in_state_4 _ =
    let init_state = DummyState2A_Parsable.A ((1,0),(2,0))
    and state_trans_func_1 = {DummyState2A_Parsable.func= DummyState2A_Parsable.fun_1;transition_idx=3} 
    and state_trans_func_2 = {DummyState2A_Parsable.func= DummyState2A_Parsable.fun_3;transition_idx=7} in
    let init_situation_in_state = DF.SSP.init_situation_in_state (DF.SS.init_situation init_state) 
    and courses_between_states_0_1 = DF.SSP.Courses (Seq.return state_trans_func_1)
    and courses_between_states_1_2 = DF.SSP.Courses (Seq.return state_trans_func_2) in
    let init_situation_matrix = DF.SSP.init_situation_matrix init_situation_in_state ~state_idx:0 ~num_of_states:3 in
    let trans_matrix = Policy.Square_matrix.init_single DF.SSP.No_transitions 3
    and num_of_steps = 777 in
    Square_matrix.update trans_matrix ~row:0 ~column:1 courses_between_states_0_1;
    Square_matrix.update trans_matrix ~row:1 ~column:2 courses_between_states_1_2;
    let result_sits_matrix,steps,is_reached = DF.search_for_situation_in_state init_situation_matrix trans_matrix ~state_idx:2 ~max_num_of_steps:num_of_steps 
    and expected_sits_matrix = Array.make 3 DF.SSP.Not_reachable
    and expected_num_of_steps = 2
    and expected_reachability_flag = true in
    Array.set expected_sits_matrix 2 (DF.SSP.Situations (Seq.return { DF.SS.current_state=DummyState2A_Parsable.A ((1,2),(2,3));current_walk=[state_trans_func_2;state_trans_func_1] } ));
    assert_equal
        ~msg:("Result should be computed in "^(string_of_int num_of_steps)^" steps")
        ~printer:(fun i -> string_of_int i)
        expected_num_of_steps
        steps;
    assert_equal
        ~msg:"State suppose to be unreachable" 
        expected_reachability_flag
        is_reached;
    assert_equal
        ~msg:"Result situation matrix is not equal to expected"
        ~printer: _situation_matrix_2_string
        ~cmp:_compare_array_of_situations_in_state
        expected_sits_matrix
        result_sits_matrix 
let test_export_walk_1 _ =
    let raw_trans_funs = [
        {State.permutation_with_time_shift=[(1,2);(3,2);(2,0);(4,0)];react_label="r1";from_idx=0;to_idx=1;transition_idx=1};
        {permutation_with_time_shift=[(1,0);(2,0);(3,0);(4,1)];react_label="r3";from_idx=1;to_idx=2;transition_idx=3};
        {permutation_with_time_shift=[(4,0);(3,3);(2,3);(1,0)];react_label="r2";from_idx=2;to_idx=0;transition_idx=2}
    ]
    and walk = [
        { DummyState2A_Parsable.func = (fun s -> s);transition_idx=1 };
        { DummyState2A_Parsable.func = (fun s -> s);transition_idx=2 }
    ] in
    let result = DF.export_walk walk raw_trans_funs in
    assert_equal
        ~msg:"Exported walk is not equal to expected"
        [List.nth raw_trans_funs 0; List.nth raw_trans_funs 2]
        result

let suite =
    "Frontend tests" >::: [
        "Import trans funs test 1">:: test_import_trans_funs_1;
        "Export trans funs test 1">:: test_export_trans_funs_1;
        "Import destination states test 1">:: test_import_dest_states_1;
        "Transition matrix generation test 1">:: test_make_transformation_matrix_1;
        "State reachability test 1">:: test_is_state_reached_1;
        "State reachability test 2">:: test_is_state_reached_2;
        "Search for situations in a state test 1 ">:: test_search_for_situation_in_state_1;
        "Search for situations in a state test 2 - won't reach the desired state - no transition to this state">:: test_search_for_situation_in_state_2;
        "Search for situations in a state test 3 - won't reach the desired state - conditional transition">:: test_search_for_situation_in_state_3;
        "Search for situations in a state test 4 - desired state reach in more than one step">:: test_search_for_situation_in_state_4;
        "Walk export test 1">:: test_export_walk_1
    ]

let () =
  run_test_tt_main suite