
type blas_expr =
  | Dot of float array * float array * float
  | Swap of float array * float array

(* some comments *)

let inc blas_expr i new_value =
  match blas_expr with
  | Dot (xs, ys, prev) ->
    let x0 = xs.(i) in
    let y0 = ys.(i) in
    let curr = prev +. y0 *. (new_value -. x0) in
    let () = xs.(i) <- new_value in
    Dot (xs, ys, curr)

  | Swap (xs, ys) ->
    let tmp = Array.get xs i in
    let () = xs.(i) <- ys.(i) in
    let () = ys.(i) <- tmp in
    Swap (xs, ys)
