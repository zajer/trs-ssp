open OUnit2
open Ssp
module DSS = State_space.Make(DummyState2A)
module DSSP = State_space_policy.Make(DSS)
let _compare_situations s1 s2 =
    s1.DSS.current_state = s2.DSS.current_state && 
    if List.compare_lengths s1.current_walk s2.current_walk = 0 then
        List.for_all2 (fun we1 we2 -> we1.DummyState2A.transition_idx = we2.DummyState2A.transition_idx) s1.current_walk s2.current_walk
    else
        false
let _compare_list_of_situations los1 los2 =
    List.for_all 
        (
            fun s1 -> 
                List.exists (fun s2 -> _compare_situations s1 s2) los2
        )
        los1
let _state_2_string ((ai1,t1),(ai2,t2)) =
    "("^(string_of_int ai1)^","^(string_of_int t1)^"),"^
    "("^(string_of_int ai2)^","^(string_of_int t2)^")"
let _trans_func_2_string stf =
    string_of_int stf.DummyState2A.transition_idx
let _walk_2_string w = 
    let result = List.map (fun stf -> _trans_func_2_string stf ) w |> String.concat "," in
    "["^result^"]"
let _situation_2_string s = 
    let state = s.DSS.current_state |> _state_2_string
    and walk = s.current_walk |> _walk_2_string in
    "{"^state^","^walk^"}"
let _list_of_situations_2_string = (fun los -> List.map (fun s-> _situation_2_string s) los |> String.concat "," )
let test_convolute_1 _ =
    let state = (1,0),(2,0) 
    and state_trans_func = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a2,t2+7),(a1,t1+3)));transition_idx=3} in
    let sitution = DSS.init_situation state in
    let situation_in_state = (DSSP.init_situation_in_state sitution)
    and courses_between_situations = DSSP.Courses (Seq.return state_trans_func) in
    let result = DSSP.convolute (fun s -> Some s) situation_in_state courses_between_situations 
    and expected_result = {DSS.current_state=(2,7),(1,3);current_walk=[state_trans_func]} in
    let result_as_list = match result with
    | DSSP.Situations s -> List.of_seq s
    | DSSP.Not_reachable -> assert_failure "Convolution should produce situations but results in not reachable situation"
    in
    assert_equal 
        ~msg:"Number of reachable situations is not equal to expacted" 
        ~printer:(fun i-> string_of_int i)
        1
        (List.length result_as_list);
    assert_equal
        ~msg:"Result situation is not equal to expected"
        ~printer:_situation_2_string
        ~cmp:_compare_situations
        expected_result
        (List.hd result_as_list)
let test_convolute_2 _ =
    let state = (1,15),(2,1) 
    and state_trans_func_1 = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a2,t2+7),(a1,t1+3)));transition_idx=3} 
    and state_trans_func_2 = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a1,t1+4),(a2,t2+5)));transition_idx=7} 
    in
    let sitution = DSS.init_situation state in
    let situation_in_state = (DSSP.init_situation_in_state sitution)
    and courses_between_situations = DSSP.Courses (List.to_seq [state_trans_func_1;state_trans_func_2]) in
    let result = DSSP.convolute  (fun s -> Some s) situation_in_state courses_between_situations 
    and expected_results = [
        {DSS.current_state=(2,8),(1,18);current_walk=[state_trans_func_1]};
        {DSS.current_state=(1,19),(2,6);current_walk=[state_trans_func_2]}
        ] in
    let result_as_list = match result with
    | DSSP.Situations s -> List.of_seq s
    | DSSP.Not_reachable -> assert_failure "Convolution should produce situations but results in not reachable situation"
    in
    assert_equal 
        ~msg:"Number of reachable situations is not equal to expacted" 
        ~printer:(fun i-> string_of_int i)
        2
        (List.length result_as_list);
    assert_equal
        ~msg:"Result situations are not equal to expected"
        ~printer:_list_of_situations_2_string
        ~cmp:_compare_list_of_situations
        expected_results
        result_as_list
let test_convolute_3 _ =
    let state_1 = (1,1),(2,3) 
    and state_2 = (2,1),(1,3) 
    and state_trans_func = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a2,t2+7),(a1,t1+3)));transition_idx=3} in
    let situtions = [{DSS.current_state=state_1;current_walk=[] };{DSS.current_state=state_2;current_walk=[] }] in
    let situations_in_state = DSSP.Situations (List.to_seq situtions)
    and courses_between_situations = DSSP.Courses (Seq.return state_trans_func) in
    let result = DSSP.convolute  (fun s -> Some s) situations_in_state courses_between_situations 
    and expected_results = [
        {DSS.current_state=(2,10),(1,4);current_walk=[state_trans_func]};
        {DSS.current_state=(1,10),(2,4);current_walk=[state_trans_func]}
        ]in
    let result_as_list = match result with
    | DSSP.Situations s -> List.of_seq s
    | DSSP.Not_reachable -> assert_failure "Convolution should produce situations but results in not reachable situation"
    in
    assert_equal 
        ~msg:"Number of reachable situations is not equal to expacted" 
        ~printer:(fun i-> string_of_int i)
        2
        (List.length result_as_list);
    assert_equal
        ~msg:"Result situations is not equal to expected"
        ~printer:_list_of_situations_2_string
        ~cmp:_compare_list_of_situations
        expected_results
        result_as_list
let test_convolute_4 _ =
    let state_1 = (1,1),(2,3) 
    and state_2 = (2,7),(1,5) 
    and state_trans_func_1 = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a2,t2+7),(a1,t1+3)));transition_idx=3} 
    and state_trans_func_2 = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a1,t1+4),(a2,t2+5)));transition_idx=7}  in
    let situtions = [{DSS.current_state=state_1;current_walk=[] };{DSS.current_state=state_2;current_walk=[] }] in
    let situations_in_state = DSSP.Situations (List.to_seq situtions)
    and courses_between_situations = DSSP.Courses (List.to_seq [state_trans_func_1;state_trans_func_2]) in
    let result = DSSP.convolute  (fun s -> Some s) situations_in_state courses_between_situations 
    and expected_results = [
        {DSS.current_state=(2,10),(1,4);current_walk=[state_trans_func_1]};
        {DSS.current_state=(1,5),(2,8);current_walk=[state_trans_func_2]};
        {DSS.current_state=(1,12),(2,10);current_walk=[state_trans_func_1]};
        {DSS.current_state=(2,11),(1,10);current_walk=[state_trans_func_2]}
        ]in
    let result_as_list = match result with
    | DSSP.Situations s -> List.of_seq s
    | DSSP.Not_reachable -> assert_failure "Convolution should produce situations but results in not reachable situation"
    in
    assert_equal 
        ~msg:"Number of reachable situations is not equal to expacted" 
        ~printer:(fun i-> string_of_int i)
        4
        (List.length result_as_list);
    assert_equal
        ~msg:"Result situations is not equal to expected"
        ~printer:_list_of_situations_2_string
        ~cmp:_compare_list_of_situations
        expected_results
        result_as_list
let test_convolute_5 _ =
    let state = (1,1),(2,0) 
    and state_trans_func = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> if t1 = t2 then ((a2,t2+7),(a1,t1+3)) else (-1,-1),(-1,-1) );transition_idx=3} in
    let sitution = DSS.init_situation state in
    let situation_in_state = (DSSP.init_situation_in_state sitution)
    and courses_between_situations = DSSP.Courses (Seq.return state_trans_func) in
    let result = DSSP.convolute 
        (fun s -> 
            match s.current_state with 
            | (-1,-1),(-1,-1) -> None
            | _ -> Some s  
        )
        situation_in_state 
        courses_between_situations in    
    let result_as_list = match result with
    | DSSP.Situations s -> List.of_seq s
    | DSSP.Not_reachable -> assert_failure "Convolution should produce situations but results in not reachable situation"
    in
    assert_equal 
        ~msg:"Number of reachable situations is not equal to expacted" 
        ~printer:(fun i-> string_of_int i)
        0
        (List.length result_as_list)
let _extract_situations situations_in_state = 
    match situations_in_state with 
    | DSSP.Not_reachable -> []
    | DSSP.Situations s -> List.of_seq s
let _check_reachability_of_state situations_in_state =
    match situations_in_state with 
    | DSSP.Not_reachable -> false
    | DSSP.Situations _ -> true
let _trans_matrix_init_fun ~row ~column = ignore row; ignore column; DSSP.No_transitions
let test_multiply_1 _ =
    let init_state = (1,0),(2,0) 
    and state_trans_func_1 = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a2,t2+7),(a1,t1+3)));transition_idx=3} 
    and state_trans_func_2 = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a1,t1+4),(a2,t2+5)));transition_idx=7} in
    let init_situation_in_state = DSSP.init_situation_in_state (DSS.init_situation init_state) 
    and courses_between_states_0_1 = DSSP.Courses (Seq.return state_trans_func_1)
    and courses_between_states_0_2 = DSSP.Courses (Seq.return state_trans_func_2) in
    let init_situation_matrix = DSSP.init_situation_matrix init_situation_in_state ~state_idx:0 ~num_of_states:3 in
    let trans_matrix = Square_matrix.init _trans_matrix_init_fun 3 in
    Square_matrix.update trans_matrix ~row:0 ~column:1 courses_between_states_0_1;
    Square_matrix.update trans_matrix ~row:0 ~column:2 courses_between_states_0_2;
    let result = DSSP.multiply  (fun s -> Some s) init_situation_matrix trans_matrix in    
    let sits_in_state_0 = Array.get result 0 |> _extract_situations
    and sits_in_state_1 = Array.get result 1 |> _extract_situations
    and sits_in_state_2 = Array.get result 2 |> _extract_situations in
    assert_equal 
        ~msg:"Number of situations in the state 0 is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        0
        (List.length sits_in_state_0);
    assert_equal 
        ~msg:"Number of situations in the state 1 is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        1
        (List.length sits_in_state_1);
    assert_equal 
        ~msg:"Number of situations in the state 2 is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        1
        (List.length sits_in_state_2);
    assert_equal
        ~msg:"Situation in the state 1 is not equal to expected"
        ~printer:_list_of_situations_2_string
        ~cmp:_compare_list_of_situations
        [ { DSS.current_state=((2,7),(1,3));current_walk=[state_trans_func_1] } ]
        sits_in_state_1;
    assert_equal
        ~msg:"Situation in the state 2 is not equal to expected"
        ~printer:_list_of_situations_2_string
        ~cmp:_compare_list_of_situations
        [ { DSS.current_state=((1,4),(2,5));current_walk=[state_trans_func_2] } ]
        sits_in_state_2
let test_multiply_2 _ =
    let init_state_1 = (1,0),(2,0) 
    and init_state_2 = (2,7),(1,5) 
    and state_trans_func_1 = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a2,t2+7),(a1,t1+3)));transition_idx=3} 
    and state_trans_func_2 = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a1,t1+4),(a2,t2+5)));transition_idx=7} in
    let init_situation_in_state = DSSP.Situations (List.to_seq [{DSS.current_state=init_state_1;current_walk =[]};{current_state=init_state_2;current_walk=[]}])
    and courses_between_states_0_1 = DSSP.Courses (Seq.return state_trans_func_1)
    and courses_between_states_0_2 = DSSP.Courses (Seq.return state_trans_func_2) in
    let init_situation_matrix = DSSP.init_situation_matrix init_situation_in_state ~state_idx:0 ~num_of_states:3 in
    let trans_matrix = Square_matrix.init _trans_matrix_init_fun 3 in
    Square_matrix.update trans_matrix ~row:0 ~column:1 courses_between_states_0_1;
    Square_matrix.update trans_matrix ~row:0 ~column:2 courses_between_states_0_2;
    let result = DSSP.multiply  (fun s -> Some s) init_situation_matrix trans_matrix in    
    let sits_in_state_0 = Array.get result 0 |> _extract_situations
    and sits_in_state_1 = Array.get result 1 |> _extract_situations
    and sits_in_state_2 = Array.get result 2 |> _extract_situations in
    assert_equal 
        ~msg:"Number of situations in the state 0 is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        0
        (List.length sits_in_state_0);
    assert_equal 
        ~msg:"Number of situations in the state 1 is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        2
        (List.length sits_in_state_1);
    assert_equal 
        ~msg:"Number of situations in the state 2 is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        2
        (List.length sits_in_state_2);
    assert_equal
        ~msg:"Situations in the state 1 are not equal to expected"
        ~printer:_list_of_situations_2_string
        ~cmp:_compare_list_of_situations
        [ 
            { DSS.current_state=((2,7),(1,3));current_walk=[state_trans_func_1] }; 
            { DSS.current_state=((1,12),(2,10));current_walk=[state_trans_func_1] }
        ]
        sits_in_state_1;
    assert_equal
        ~msg:"Situations in the state 2 are not equal to expected"
        ~printer:_list_of_situations_2_string
        ~cmp:_compare_list_of_situations
        [ 
            { DSS.current_state=((1,4),(2,5));current_walk=[state_trans_func_2] } ;
            { DSS.current_state=((2,11),(1,10));current_walk=[state_trans_func_2] } 
        ]
        sits_in_state_2
let test_multiply_3 _ =
    let init_state_1 = (1,0),(2,0) 
    and init_state_2 = (2,7),(1,5) 
    and state_trans_func_1 = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> if t1 = t2 then ((a2,t2+7),(a1,t1+3)) else (-1,-1),(-1,-1) );transition_idx=3} 
    and state_trans_func_2 = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a1,t1+4),(a2,t2+5)));transition_idx=7} in
    let init_situation_in_state = DSSP.Situations (List.to_seq [{DSS.current_state=init_state_1;current_walk =[]};{current_state=init_state_2;current_walk=[]}])
    and courses_between_states_0_1 = DSSP.Courses (Seq.return state_trans_func_1)
    and courses_between_states_0_2 = DSSP.Courses (Seq.return state_trans_func_2) in
    let init_situation_matrix = DSSP.init_situation_matrix init_situation_in_state ~state_idx:0 ~num_of_states:3 in
    let trans_matrix = Square_matrix.init _trans_matrix_init_fun 3 in
    Square_matrix.update trans_matrix ~row:0 ~column:1 courses_between_states_0_1;
    Square_matrix.update trans_matrix ~row:0 ~column:2 courses_between_states_0_2;
    let result = DSSP.multiply 
        (fun s -> 
            match s.current_state with 
            | (-1,-1),(-1,-1) -> None
            | _ -> Some s  
        ) 
        init_situation_matrix 
        trans_matrix in    
    let sits_in_state_0 = Array.get result 0 |> _extract_situations
    and sits_in_state_1 = Array.get result 1 |> _extract_situations
    and sits_in_state_2 = Array.get result 2 |> _extract_situations in
    assert_equal 
        ~msg:"Number of situations in the state 0 is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        0
        (List.length sits_in_state_0);
    assert_equal 
        ~msg:"Number of situations in the state 1 is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        1
        (List.length sits_in_state_1);
    assert_equal 
        ~msg:"Number of situations in the state 2 is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        2
        (List.length sits_in_state_2);
    assert_equal
        ~msg:"Situations in the state 1 are not equal to expected"
        ~printer:_list_of_situations_2_string
        ~cmp:_compare_list_of_situations
        [ 
            { DSS.current_state=((2,7),(1,3));current_walk=[state_trans_func_1] }; 
        ]
        sits_in_state_1;
    assert_equal
        ~msg:"Situations in the state 2 are not equal to expected"
        ~printer:_list_of_situations_2_string
        ~cmp:_compare_list_of_situations
        [ 
            { DSS.current_state=((1,4),(2,5));current_walk=[state_trans_func_2] } ;
            { DSS.current_state=((2,11),(1,10));current_walk=[state_trans_func_2] } 
        ]
        sits_in_state_2
let test_multiply_4 _ = 
    let init_state = (1,0),(2,0) 
    and state_trans_func_1 = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a2,t2+7),(a1,t1+3)));transition_idx=3} 
    and state_trans_func_2 = {DummyState2A.func= (fun ((a1,t1),(a2,t2)) -> ((a1,t1+4),(a2,t2+5)));transition_idx=7} in
    let init_situation_in_state = DSSP.init_situation_in_state (DSS.init_situation init_state) 
    and courses_between_states_0_1 = DSSP.Courses (Seq.return state_trans_func_1)
    and courses_between_states_1_2 = DSSP.Courses (Seq.return state_trans_func_2) in
    let init_situation_matrix = DSSP.init_situation_matrix init_situation_in_state ~state_idx:0 ~num_of_states:3 in
    let trans_matrix = Square_matrix.init _trans_matrix_init_fun 3 in
    Square_matrix.update trans_matrix ~row:0 ~column:1 courses_between_states_0_1;
    Square_matrix.update trans_matrix ~row:1 ~column:2 courses_between_states_1_2;
    let result = DSSP.multiply  (fun s -> Some s) init_situation_matrix trans_matrix in    
    let sits_in_state_0 = Array.get result 0 |> _extract_situations
    and sits_in_state_1 = Array.get result 1 |> _extract_situations
    and sits_in_state_2 = Array.get result 2 |> _extract_situations in
    assert_equal 
        ~msg:"Number of situations in the state 0 is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        0
        (List.length sits_in_state_0);
    assert_equal 
        ~msg:"Number of situations in the state 1 is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        1
        (List.length sits_in_state_1);
    assert_equal 
        ~msg:"Number of situations in the state 2 is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        0
        (List.length sits_in_state_2)
let suite =
"State space policy tests" >::: [

    "Convolution test 1">:: test_convolute_1;
    "Convolution test 2">:: test_convolute_2;
    "Convolution test 3">:: test_convolute_3;
    "Convolution test 4">:: test_convolute_4;
    "Convolution test 5">:: test_convolute_5;
    "Multiplication test 1">:: test_multiply_1;
    "Multiplication test 2">:: test_multiply_2;
    "Multiplication test 3 - conditional transition">:: test_multiply_3;
    "Multiplication test 4 - not all transition functions apply ">:: test_multiply_4
]

let () =
  run_test_tt_main suite