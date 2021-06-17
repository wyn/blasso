module Zipper = struct

  type el = float

  type t = {
    left_: el list;
    focus_: el;
    right_: el list;
    index_: int;
    length_: int;
  }

  let _clamp bottom top i =
    let i = min i (top-1) in
    let i = max i bottom in
    i

  let get t = t.focus_

  let set {left_; focus_=_; right_; index_; length_} ~value = {
    left_;
    focus_=value;
    right_;
    index_;
    length_;
  }

  let of_array arr ~index =
    let n = Array.length arr in
    let i = _clamp 0 n index in {
      left_=Array.sub arr 0 i |> Array.to_list |> List.rev;
      focus_=arr.(i);
      right_=Array.sub arr (i+1) (n-(i+1)) |> Array.to_list;
      index_=i;
      length_=n;
    }

  let to_array {left_; focus_; right_; _} =
    let left_ = left_ |> List.rev |> Array.of_list in
    let right_ = right_ |> Array.of_list in
    Array.concat [left_; [|focus_|]; right_]

  let shift_left t =
    match t.left_ with
    | [] -> t
    | new_focus_ :: new_left_ ->
      let new_right_ = t.focus_ :: t.right_ in {
        left_=new_left_;
        focus_=new_focus_;
        right_=new_right_;
        index_=(t.index_ - 1);
        length_=t.length_;
      }

  let shift_right t =
    match t.right_ with
    | [] -> t
    | new_focus_ :: new_right_ ->
      let new_left_ = t.focus_ :: t.left_ in {
        left_=new_left_;
        focus_=new_focus_;
        right_=new_right_;
        index_=(t.index_ + 1);
        length_=t.length_;
      }

  let rec jump_to t ~index =
    let n = t.length_ in
    let new_index = _clamp 0 n index in
    let i = t.index_ in
    match compare i new_index with
    | 0 -> t
    | 1 -> let t = shift_left t in jump_to t ~index:new_index
    | _ -> let t = shift_right t in jump_to t ~index:new_index



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
