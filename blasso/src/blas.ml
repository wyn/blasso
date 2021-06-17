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
      left_=Array.sub arr 0 i
            |> Array.to_list
            |> List.rev;
      focus_=arr.(i);
      right_=Array.sub arr (i+1) (n-(i+1))
             |> Array.to_list;
      index_=i;
      length_=n;
    }

  let to_array {left_; focus_; right_; _} =
    let left_ = left_
                |> List.rev
                |> Array.of_list in
    let right_ = right_
                 |> Array.of_list in
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

    | 1 -> shift_left t
           |> jump_to ~index:new_index

    | _ -> shift_right t
           |> jump_to ~index:new_index



end

(* NOTE
 *
 * always updating xs at one index location with a new value,
 * and also updating the result with the appropriate new calc value/values
 *
 *  If the result hasnt been set (=None) then do full calculation first
 *  and then update *)

module Dot_data = struct
  type t = {
    xs_: Zipper.t;
    ys_: Zipper.t;
    result_: float option; (* not really a float, its a list of sums of pointwise multiplies *)
  }

  let init xs ys = {
    xs_=xs |> Zipper.of_array ~index:0;
    ys_=ys |> Zipper.of_array ~index:0;
    result_=None;
  }

  let result t = t.result_

  let rec update {xs_; ys_; result_} ~index ~value =
    match result_ with
    | Some prev ->
      let xs_at_i = xs_ |> Zipper.jump_to ~index in
      let x0 = xs_at_i |> Zipper.get in
      let ys_at_i = ys_ |> Zipper.jump_to ~index in
      let y0 = ys_at_i |> Zipper.get in
      let result_ = Some (prev +. y0 *. (value -. x0)) in
      {
        xs_=xs_at_i |> Zipper.set ~value;
        ys_=ys_at_i;
        result_;
      }
    | None ->
      let xarr = xs_ |> Zipper.to_array in
      let yarr = ys_ |> Zipper.to_array in
      let result_ = Some (
          yarr
          |> Array.map2 (fun x y -> x *. y) xarr
          |> Array.fold_left (fun acc v -> acc +. v) 0.
        ) in
      update {xs_; ys_; result_} ~index ~value

end


module Swap_data = struct

  type xsys = {
    xs_: Zipper.t;
    ys_: Zipper.t;
  }

  type t = {
    xs_: Zipper.t;
    ys_: Zipper.t;
    result_: xsys option;
  }

  let rec update {xs_; ys_; result_} ~index ~value =
    match result_ with
    | Some prev ->
      let xs_ = xs_
                |> Zipper.jump_to ~index
                |> Zipper.set ~value in
      let ys_ = ys_
                |> Zipper.jump_to ~index in
      let prev_xs = prev.xs_
                    |> Zipper.jump_to ~index in
      let prev_ys = prev.ys_
                    |> Zipper.jump_to ~index
                    |> Zipper.set ~value in
      let result_ = Some {
          xs_=prev_xs;
          ys_=prev_ys
        } in
      {xs_; ys_; result_}
    | None ->
      let result_ = Some {
          xs_=ys_ |> Zipper.to_array |> Zipper.of_array ~index;
          ys_=xs_ |> Zipper.to_array |> Zipper.of_array ~index;
        } in
      update {xs_; ys_; result_} ~index ~value
end

module Scale_data = struct

  type t = {
    xs_: Zipper.t;
    alpha_: float;
    result_: Zipper.t option;
  }

  let rec update {xs_; alpha_; result_} ~index ~value =
    match result_ with
    | Some prev ->
      let xs_ = xs_
                |> Zipper.jump_to ~index
                |> Zipper.set ~value in
      let chc_at_i = prev |> Zipper.jump_to ~index in
      let result_ = Some (chc_at_i |> Zipper.set ~value:(value *. alpha_)) in
      {xs_; alpha_; result_}
    | None ->
      let result_ = Some (
          xs_
          |> Zipper.to_array
          |> Array.map (fun x -> x *. alpha_)
          |> Zipper.of_array ~index
        ) in
      update {xs_; alpha_; result_} ~index ~value

end

module Copy_data = struct

  type t = {
    xs_: Zipper.t;
    result_: Zipper.t option;
  }

  let rec update {xs_; result_} ~index ~value =
    match result_ with
    | Some prev ->
      let xs_ = xs_
                |> Zipper.jump_to ~index
                |> Zipper.set ~value in
      let result_ = Some (
          prev
          |> Zipper.jump_to ~index
          |> Zipper.set ~value
        ) in
      {xs_; result_}
    | None ->
      let result_ = Some (
          xs_
          |> Zipper.to_array
          |> Zipper.of_array ~index
        ) in
      update {xs_; result_} ~index ~value

end

type blas_expr =
  | Dot of Dot_data.t
  | Swap of Swap_data.t
  | Scale of Scale_data.t
  | Copy of Copy_data.t

(* some comments *)

let update blas_expr ~index ~value =
  match blas_expr with
  | Dot data -> Dot (Dot_data.update data ~index ~value)
  | Swap data -> Swap (Swap_data.update data ~index ~value)
  | Scale data -> Scale (Scale_data.update data ~index ~value)
  | Copy data -> Copy (Copy_data.update data ~index ~value)
