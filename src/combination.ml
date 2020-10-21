
module type State =
  sig
  type i
  type t
  type k = t list
  type f = t -> int -> t
  val i_to_string : i -> string
  val k_to_string : k -> string
end

module StatesCombination( S:State ) = 
  struct
  type t = S.t
  
  type k_matrix = { matrix:S.k array; step:int}
  type f_matrix = { matrix:S.f list array array; num_of_states:int}
  let f_at_cs (f:S.f) k t =
    List.fold_left (fun res c -> f c t :: res) [] k
  let convolute k f_list t =
    List.fold_left (fun res f -> f_at_cs f k t @ res) [] f_list
  let array_out_of_column fm column =
    let res_as_list = Array.fold_left (fun res_as_list row -> Array.get row column :: res_as_list) [] fm.matrix
    in
      List.rev res_as_list |> Array.of_list
  let multiply ?(filter_fun= fun _ -> true) (km:k_matrix) fm =
    let res_as_list_reversed,_ = Array.fold_left 
      (fun (result_as_list,i) _ -> 
        let column_of_f_series = array_out_of_column fm i in
        let new_k_elem,_ = 
          Array.fold_left (fun (res,i) k_series -> 
            let part_res = convolute k_series (Array.get column_of_f_series i) km.step in 
            res@part_res,i+1
          )
          ([],0) 
          km.matrix in
            (new_k_elem |> List.filter filter_fun ):: result_as_list,i+1
      )
      ([],0) 
      (Array.make fm.num_of_states [])
    in
      { matrix=Array.of_list (res_as_list_reversed |> List.rev );step = km.step+1}
  let print_km mk = 
    Array.iteri (fun i k-> print_endline ("kolumna "^ string_of_int i) ; S.k_to_string k |> print_string ; print_newline () ) mk
end


