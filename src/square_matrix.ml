type 'a t = { elements: 'a array array; length:int}

let length matrix = matrix.length
let _get_element_in_ith_row_jth_column matrix ~i ~j =
    let row = Array.get matrix.elements i in
    let element = Array.get row j in
        element
let column matrix column =
    let result = Array.init matrix.length  (fun i -> _get_element_in_ith_row_jth_column ~i:i ~j:column matrix ) in
        result
let make elts = 
    let number_of_rows = Array.length elts in
    let are_elts_valid = Array.fold_left (fun res row -> (Array.length row = number_of_rows) && res) true elts in
    if are_elts_valid then
        {elements=elts;length=number_of_rows}
    else
        raise (Invalid_argument "Provided elements do not create square matrix")