open OUnit2
open Ssp
module TemplateState = Template_state;;
TemplateState._number_of_agents := 4;;
TemplateState._number_of_states := 3;;
let test_parse_trans_funs _ =
    let trans_funs_file = "trans_funs.csv" in
    let parsed_trans_funs = Frontend.import_trans_funs trans_funs_file |> List.map TemplateState.parse_trans_fun
    in
    let test_input_1 = [|(1,0);(2,0);(3,0);(4,0)|]
    and test_input_2 = [|(4,7);(2,5);(1,9);(3,20)|]
    and test_input_3 = [|(3,0);(2,1);(1,0);(4,0)|] in
    let expected_output_1 = [|(1,2);(3,2);(2,0);(4,0)|]
    and expected_output_2 = [|(4,7);(2,5);(1,9);(3,21)|]
    and expected_output_3 = [|(-1,-1);(-1,-1);(-1,-1);(-1,-1)|] in
    assert_equal
        ~msg:"Result 1 of parsed function application is not equal to expected"
        ~printer:TemplateState.to_stirng
            expected_output_1
            ((List.nth parsed_trans_funs 0).func test_input_1);
    assert_equal
        ~msg:"Result 2 of parsed function application is not equal to expected"
        ~printer:TemplateState.to_stirng
            expected_output_2
            ((List.nth parsed_trans_funs 1).func test_input_2);
    assert_equal
        ~msg:"Result 3 of parsed function application is not equal to expected"
        ~printer:TemplateState.to_stirng
            expected_output_3
            ((List.nth parsed_trans_funs 2).func test_input_3)
let test_num_of_states _ = 
    let expected_number_of_states = 3 
    and number_of_states = TemplateState.num_of_states () in
    assert_equal
        ~msg:"Number of states not equal to expected"
        ~printer:(fun i -> string_of_int i)
        expected_number_of_states
        number_of_states
let test_is_negligible_1 _ = 
    let state = [|(-1,-1);(-1,-1)|] in
    let result = TemplateState.is_negligible state
    and expected_result = true in
    assert_equal
        ~msg:"The provided state should be considered negigible"
        expected_result
        result
let test_is_negligible_2 _ = 
    let state = [|(-1,-1);(-1,-1);(-1,-1);(-1,-1)|] in
    let result = TemplateState.is_negligible state
    and expected_result = true in
    assert_equal
        ~msg:"The provided state should be considered negigible"
        expected_result
        result
let test_is_negligible_3 _ = 
    let state = [|(4,7);(2,5);(1,9);(3,21)|] in
    let result = TemplateState.is_negligible state
    and expected_result = false in
    assert_equal
        ~msg:"The provided state should not be considered negigible"
        expected_result
        result
let test_is_negligible_4 _ = 
    let state = [|(4,7);(2,5)|] in
    let result = TemplateState.is_negligible state
    and expected_result = false in
    assert_equal
        ~msg:"The provided state should not be considered negigible"
        expected_result
        result
let suite =
    "Template state tests" >::: [
        "Parsing test ">:: test_parse_trans_funs;
        "Number of states test ">:: test_num_of_states;
        "Negligiblity test 1">:: test_is_negligible_1;
        "Negligiblity test 2">:: test_is_negligible_2;
        "Negligiblity test 3">:: test_is_negligible_3;
        "Negligiblity test 4">:: test_is_negligible_4; 
    ]

let () =
  run_test_tt_main suite