open OUnit2
module DSS = Policy.State_space.Make(DummyState2A)
module DSSP = Policy.State_space_policy.Make(DSS)
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
    let result = DSSP.convolute situation_in_state courses_between_situations 
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
    let result = DSSP.convolute situation_in_state courses_between_situations 
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
    let result = DSSP.convolute situations_in_state courses_between_situations 
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
    let result = DSSP.convolute situations_in_state courses_between_situations 
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
let suite =
"State space policy tests" >::: [
    "Convolution test 1">:: test_convolute_1;
    "Convolution test 2">:: test_convolute_2;
    "Convolution test 3">:: test_convolute_3;
    "Convolution test 4">:: test_convolute_4
]

let () =
  run_test_tt_main suite