open OUnit2
open Ssp

let _array_of_ints_to_string = fun aoi -> Array.map (fun i -> string_of_int i) aoi |> Array.to_list |> String.concat ";"
let _compare_arrays_of_arrays_of_ints = fun aoaoi1 aoaoi2 -> Array.for_all2 ( fun row1 row2 -> Array.for_all2 (fun i1 i2 -> i1 = i2) row1 row2 ) aoaoi1 aoaoi2
let test_get_column_1 _ = 
    let matrix_elts = Array.init 3 (fun row -> (Array.init 3 (fun column -> row*3+column) )) in
    let matrix = Square_matrix.make matrix_elts in
    let expected_column_1 = [|0;3;6|]
    and expected_column_2 = [|1;4;7|]
    and expected_column_3 = [|2;5;8|]
    and result_column_1 = Square_matrix.column matrix 0
    and result_column_2 = Square_matrix.column matrix 1
    and result_column_3 = Square_matrix.column matrix 2 in
    assert_equal
        ~msg:"First column is not equal to expected"
        ~printer:_array_of_ints_to_string
        expected_column_1
        result_column_1;
    assert_equal
        ~msg:"Second column is not equal to expected"
        ~printer:_array_of_ints_to_string
        expected_column_2
        result_column_2;
    assert_equal
        ~msg:"Third column is not equal to expected"
        ~printer:_array_of_ints_to_string
        expected_column_3
        result_column_3
let test_update_1 _ = 
    let matrix_elts = Array.init 3 (fun row -> (Array.init 3 (fun column -> row*3+column) )) in
    let matrix = Square_matrix.make matrix_elts in
    let expected_matrix = [|[|123;1;2|];[|3;456;5|];[|6;7;789|]|] in
    Square_matrix.update matrix ~row:0 ~column:0 123;
    Square_matrix.update matrix ~row:1 ~column:1 456;
    Square_matrix.update matrix ~row:2 ~column:2 789;
    assert_equal 
        ~msg:"Result matrix is not equal to expected"
        ~cmp:_compare_arrays_of_arrays_of_ints
        expected_matrix
        matrix.elements
let test_update_2 _ = 
    let matrix_elts = Array.init 3 (fun row -> (Array.init 3 (fun column -> row*3+column) )) in
    let matrix = Square_matrix.make matrix_elts in
    let expected_matrix = [|[|0;1;2|];[|3;4;777|];[|6;7;8|]|] in
    Square_matrix.update matrix ~row:1 ~column:2 777;
    assert_equal 
        ~msg:"Result matrix is not equal to expected"
        ~cmp:_compare_arrays_of_arrays_of_ints
        expected_matrix
        matrix.elements      
let suite =
"Square matrix tests" >::: [
    "Getting a column test 1">:: test_get_column_1;
    "Updating matrix test 1">:: test_update_1;
    "Updating matrix test 2">:: test_update_2
]

let () =
  run_test_tt_main suite