type blas_data = {
  xs: float array;
  ys: float array;
  value: float option
}

type blas_expr =
  | Dot of blas_data
  | Swap of blas_data

(* some comments *)

let inc blas_expr i new_value =
  match blas_expr with
  | Dot ({xs; ys; value}) -> (
      match value with
      | Some prev ->
        let x0 = xs.(i) in
        let y0 = ys.(i) in
        let curr = prev +. y0 *. (new_value -. x0) in
        let () = xs.(i) <- new_value in
        Dot ({xs; ys; value=(Some curr)})
      | None -> Dot ({xs; ys; value})
    )
  | Swap ({xs; ys; value}) ->
    let tmp = Array.get xs i in
    let () = xs.(i) <- ys.(i) in
    let () = ys.(i) <- tmp in
    Swap ({xs; ys; value})
