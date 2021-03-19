type 'a t = { elements: 'a array array; length:int}

let length matrix = matrix.length
let _get_element_in_ith_row_jth_column matrix ~i ~j =
    let row = Array.get matrix.elements i in
    let element = Array.get row j in
        element
let column matrix column =
    let result = Array.init matrix.length  (fun i -> _get_element_in_ith_row_jth_column ~i:i ~j:column matrix ) in
        result