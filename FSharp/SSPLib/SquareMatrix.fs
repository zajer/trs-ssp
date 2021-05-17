namespace SSPLib

module SquareMatrix =
    type 'a t = { elements: 'a array array; length:int}

    let length matrix = matrix.length
    let _get_element_in_ith_row_jth_column matrix row column =
        let row = Array.get matrix.elements row
        let element = Array.get row column
        element
    let column matrix column =
        let result = Array.init matrix.length  (fun i -> _get_element_in_ith_row_jth_column matrix i column ) in
            result
    let make elts = 
        let number_of_rows = Array.length elts in
        let are_elts_valid = Array.fold (fun res row -> (Array.length row = number_of_rows) && res) true elts in
        if are_elts_valid then
            {elements=elts;length=number_of_rows}
        else
            raise (invalidArg "elts" "Provided elements do not create square matrix")
    let init init_fun length= 
        let elts = Array.init length (fun row -> Array.init length (fun column -> init_fun row column )) in
        {elements=elts;length=length}
    let init_single init_elem length =
        let elts = Array.init length (fun _ -> Array.init length (fun _ -> init_elem ))
        {elements=elts;length=length}
    let update matrix row column new_val = 
        Array.set 
            matrix.elements
            row 
            (
                let row_to_update = (Array.get matrix.elements row) in
                Array.set 
                    row_to_update
                    column 
                    new_val;
                    row_to_update
            )
    let to_string func matrix =
        Array.map 
            (
                fun row -> 
                    Array.map (fun e -> func e) row |> Array.toList |> String.concat "\t"
            )
            matrix.elements
        |> Array.toList |> String.concat "\n"