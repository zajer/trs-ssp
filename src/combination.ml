
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
    List.map (fun c -> f c t) k
  let convolute k f_list t =
    List.fold_right (fun f res -> f_at_cs f k t @ res) f_list []
  let array_out_of_column fm column =
    let res_as_list = Array.fold_left (fun res_as_list row -> Array.get row column :: res_as_list) [] fm.matrix
    in
      List.rev res_as_list |> Array.of_list
  let multiply ?(filter_fun= fun _ -> true) (km:k_matrix) fm =    
    let new_km_m = 
      Array.mapi 
      (
        fun column _-> 
          let column_of_fs = array_out_of_column fm column in
          let elements_of_new_k_elem = Array.mapi (fun row k_elem -> convolute k_elem ( Array.get column_of_fs row) km.step ) km.matrix in
          Array.to_list elements_of_new_k_elem |> List.flatten |> List.filter filter_fun
      ) 
      (Array.make fm.num_of_states []) in
    { matrix=new_km_m;step = km.step+1}
  let print_km mk = 
    Array.iteri (fun i k-> print_endline ("#column "^ string_of_int i) ; S.k_to_string k |> print_string ; print_newline () ) mk
end


