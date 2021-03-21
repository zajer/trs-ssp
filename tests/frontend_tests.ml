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
module DF = Frontend.Make(DummyState2A_Parsable)

let _update_value_of_square_array array ~row ~column new_val = 
    Array.set 
        array
        row 
        (
            let row_to_update = (Array.get array row) in
            Array.set 
                row_to_update
                column 
                new_val;
                row_to_update
        )
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
let test_make_transformation_matrix_1 _ =
    let matrix_input = [
        {State.permutation_with_time_shift=[(1,2);(2,0)];react_label="r1";from_idx=0;to_idx=1;transition_idx=1};
        {permutation_with_time_shift=[(2,1);(1,0)];react_label="r3";from_idx=1;to_idx=2;transition_idx=3};
        {permutation_with_time_shift=[(1,0);(2,3)];react_label="r2";from_idx=2;to_idx=0;transition_idx=2}
    ] 
    and state_trans_func_1 = {DummyState2A_Parsable.func=(fun ((a1,t1),(a2,t2)) -> (a1,t1+2),(a2,t2));transition_idx=1}
    and state_trans_func_2 = {DummyState2A_Parsable.func=(fun ((a1,t1),(a2,t2)) -> (a2,t2+1),(a1,t1));transition_idx=3} 
    and state_trans_func_3 = {DummyState2A_Parsable.func=(fun ((a1,t1),(a2,t2)) -> (a1,t1),(a2,t2+3));transition_idx=2} 
    in
    let expected_matrix_array = Array.make 3 (Array.make 3 DF.SSP.No_transitions) in
    _update_value_of_square_array expected_matrix_array ~row:0 ~column:1 (DF.SSP.Courses (Seq.return state_trans_func_1));
    _update_value_of_square_array expected_matrix_array ~row:1 ~column:2 (DF.SSP.Courses (Seq.return state_trans_func_2));
    _update_value_of_square_array expected_matrix_array ~row:2 ~column:0 (DF.SSP.Courses (Seq.return state_trans_func_3));
    let expected_matrix = Square_matrix.make expected_matrix_array
    and result_matrix = DF.make_system_transformation_matrix matrix_input in
    assert_equal
        ~msg:"Result transformation matrix is not equal to expected"
        ~cmp:_compare_transformation_matrices
        expected_matrix
        result_matrix
let suite =
"Frontend tests" >::: [
    "Import trans funs test 1">:: test_import_trans_funs_1;
    "Transition matrix generation test 1">:: test_make_transformation_matrix_1;
]

let () =
  run_test_tt_main suite