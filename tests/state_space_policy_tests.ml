open OUnit2
module DSS = Policy.State_space.Make(Dummy_state_2a)
module DSSP = Policy.State_space_policy.Make(DSS)
let _compare_situations =
    (
        fun s1 s2 -> s1.DSS.current_state = s2.DSS.current_state && 
        List.for_all2 (fun we1 we2 -> we1 == we2) s1.current_walk s2.current_walk
    )
let test_convolute_1 _ =
    let state = (1,0),(2,0) 
    and state_trans_func = {Dummy_state_2a.func= (fun ((a1,t1),(a2,t2)) -> ((a2,t2+7),(a1,t1+3)));transition_idx=3} in
    let sitution = DSS.init_situation state in
    let situation_in_state = (DSSP.init_situation_in_state sitution)
    and courses_between_situations = DSSP.Courses (Seq.return state_trans_func) in
    let result = DSSP.convolute situation_in_state courses_between_situations in
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
        ~cmp:_compare_situations
        {DSS.current_state=(2,7),(1,3);current_walk=[state_trans_func]}
        (List.hd result_as_list)
let suite =
"Test suite" >::: [
    "Convolution test 1">:: test_convolute_1
]

let () =
  run_test_tt_main suite