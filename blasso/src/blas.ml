module Zipper = struct

  type el = float

  type t = {
    left: el list;
    focus: el;
    right: el list
  }

  let of_array arr i =
    let n = Array.length arr in
    let i = min i (n-1) in
    let i = max i 0 in {
      left=Array.sub arr 0 i |> Array.to_list |> List.rev;
      focus=arr.(i);
      right=Array.sub arr (i+1) (n-(i+1)) |> Array.to_list;
    }

  let to_array {left; focus; right} =
    let left = left |> List.rev |> Array.of_list in
    let right = right |> Array.of_list in
    Array.concat [left; [|focus|]; right]

  let shift_left {left; focus; right} =
    match left with
    | [] -> {left; focus; right}
    | new_focus :: new_left ->
      let new_right = focus :: right in {
        left=new_left;
        focus=new_focus;
        right=new_right;
      }

  let shift_right {left; focus; right} =
    match right with
    | [] -> {left; focus; right}
    | new_focus :: new_right ->
      let new_left = focus :: left in {
        left=new_left;
        focus=new_focus;
        right=new_right;
      }

end

type blas_data = {
  xs: float array;
  ys: float array;
  value: float option
}

type blas_expr =
  | Dot of blas_data
  | Swap of blas_data

(* some comments *)

let rec inc blas_expr i new_value =
  match blas_expr with
  | Dot {xs; ys; value} -> (
      match value with
      | Some prev ->
        let x0 = xs.(i) in
        let y0 = ys.(i) in
        let value = Some (prev +. y0 *. (new_value -. x0)) in
        let () = xs.(i) <- new_value in
        Dot {xs; ys; value}
      | None ->
        let value = Some (
            Array.map2 (fun x y -> x *. y) xs ys |>
            Array.fold_left (fun acc v -> acc +. v) 0.
          ) in
        inc (Dot {xs; ys; value}) i new_value
    )
  | Swap {xs; ys; value} ->
    let tmp = Array.get xs i in
    let () = xs.(i) <- ys.(i) in
    let () = ys.(i) <- tmp in
    Swap {xs; ys; value}
