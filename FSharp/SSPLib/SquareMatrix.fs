namespace SSPLib

module SquareMatrix =
    type 'a matrix = { elements: 'a array array; length:int}

    let length matrix = matrix.length
    let private _get_element_in_ith_row_jth_column matrix row column =
        let row = Array.get matrix.elements row
        let element = Array.get row column
        element
    let column matrix column =
        let result = Array.init matrix.length  (fun i -> _get_element_in_ith_row_jth_column matrix i column )
        result
    let make elts = 
        let number_of_rows = Array.length elts
        let are_elts_valid = Array.fold (fun res row -> (Array.length row = number_of_rows) && res) true elts
        if are_elts_valid then
            {elements=elts;length=number_of_rows}
        else
            raise (invalidArg "elts" "Provided elements do not create square matrix")
    let init initFun length= 
        let elts = Array.init length (fun row -> Array.init length (fun column -> initFun row column ))
        {elements=elts;length=length}
    let initSingleValue initElem length =
        let elts = Array.init length (fun _ -> Array.init length (fun _ -> initElem ))
        {elements=elts;length=length}
    let update matrix row column newVal = 
        Array.set 
            matrix.elements
            row 
            (
                let row_to_update = (Array.get matrix.elements row)
                Array.set 
                    row_to_update
                    column 
                    newVal;
                    row_to_update
            )
    let toString func matrix =
        Array.map 
            (
                fun row -> 
                    Array.map (fun e -> func e) row |> Array.toList |> String.concat "\t"
            )
            matrix.elements
        |> Array.toList |> String.concat "\n"