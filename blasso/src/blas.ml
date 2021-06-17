(* NOTE
  *
  * only ever updating xs at one index location at a time
  * and also updating the result with the appropriate new calc value/values
  *
  *  If the result hasnt been set (=None) then do full calculation first
  *  and then update *)

module Z = Zipper

module Dot_data = struct

  type result = float (* not really a float, its a list of sums of pointwise multiplies *)

  type t = {
    xs_: Z.t;
    ys_: Z.t;
    result_: result option;
  }

  let init xs ys = {
    xs_=xs |> Z.of_array ~index:0;
    ys_=ys |> Z.of_array ~index:0;
    result_=None;
  }

  let result t = t.result_

  let is_ready t = result t |> Option.is_some

  let rec update {xs_; ys_; result_} ~index ~value =
    match result_ with
    | Some prev ->
      let xs_at_i = xs_ |> Z.jump_to ~index in
      let x0 = xs_at_i |> Z.get in
      let ys_at_i = ys_ |> Z.jump_to ~index in
      let y0 = ys_at_i |> Z.get in
      let result_ = Some (prev +. y0 *. (value -. x0)) in
      {
        xs_=xs_at_i |> Z.set ~value;
        ys_=ys_at_i;
        result_;
      }
    | None ->
      let xarr = xs_ |> Z.to_array in
      let yarr = ys_ |> Z.to_array in
      let result_ = Some (
          yarr
          |> Array.map2 (fun x y -> x *. y) xarr
          |> Array.fold_left (fun acc v -> acc +. v) 0.
        ) in
      update {xs_; ys_; result_} ~index ~value

end


module Swap_data = struct

  type result = {
    xs_: Z.t;
    ys_: Z.t;
  }

  type t = {
    xs_: Z.t;
    ys_: Z.t;
    result_: result option;
  }

  let init xs ys = {
    xs_=xs |> Z.of_array ~index:0;
    ys_=ys |> Z.of_array ~index:0;
    result_=None;
  }

  let result t = t.result_

  let is_ready t = result t |> Option.is_some

  let rec update {xs_; ys_; result_} ~index ~value =
    match result_ with
    | Some prev ->
      let xs_ = xs_
                |> Z.jump_to ~index
                |> Z.set ~value in
      let ys_ = ys_
                |> Z.jump_to ~index in
      let prev_xs = prev.xs_
                    |> Z.jump_to ~index in
      let prev_ys = prev.ys_
                    |> Z.jump_to ~index
                    |> Z.set ~value in
      let result_ = Some {
          xs_=prev_xs;
          ys_=prev_ys
        } in
      {xs_; ys_; result_}
    | None ->
      let result_ = Some {
          xs_=ys_ |> Z.to_array |> Z.of_array ~index;
          ys_=xs_ |> Z.to_array |> Z.of_array ~index;
        } in
      update {xs_; ys_; result_} ~index ~value
end

module Scale_data = struct

  type input = Z.t
  type output = Z.t

  type t = {
    xs_: input option;
    alpha_: float;
    result_: output option;
  }

  let init ?arr:(arr=None) ~alpha =
    match arr with
    | Some xs -> {
        xs_=Some (xs |> Z.of_array ~index:0);
        alpha_=alpha;
        result_=None;
      }
    | None -> {
        xs_=None;
        alpha_=alpha;
        result_=None
      }

  let reset t xs = {
    xs_=Some xs;
    alpha_=t.alpha_;
    result_=t.result_
  }

  let result t = t.result_

  let is_ready t = result t |> Option.is_some

  let rec update {xs_; alpha_; result_} ~index ~value =
    match (xs_, result_) with
    | (Some xs, Some prev) ->
      let xs_ = Some (
          xs
          |> Z.jump_to ~index
          |> Z.set ~value
        ) in
      let chc_at_i = prev |> Z.jump_to ~index in
      let result_ = Some (chc_at_i |> Z.set ~value:(value *. alpha_)) in
      {xs_; alpha_; result_}
    | (Some xs, None) ->
      let result_ = Some (
          xs
          |> Z.to_array
          |> Array.map (fun x -> x *. alpha_)
          |> Z.of_array ~index)
      in
      update {xs_; alpha_; result_} ~index ~value
    | (None, _) -> {xs_; alpha_; result_}

end

module Copy_data = struct

  type input = Z.t
  type output = Z.t

  type t = {
    xs_: input option;
    result_: output option;
  }

  let init ?arr:(arr=None) = {
    xs_=arr |> Option.map (Z.of_array ~index:0);
    result_=None;
  }

  let reset t xs = {
    xs_=Some xs;
    result_=t.result_
  }

  let result t = t.result_

  let is_ready t = result t |> Option.is_some

  let rec update {xs_; result_} ~index ~value =
    match (xs_, result_) with
    | (Some xs, Some prev) ->
      let xs_ = Some (
          xs
          |> Z.jump_to ~index
          |> Z.set ~value
        ) in
      let result_ = Some (
          prev
          |> Z.jump_to ~index
          |> Z.set ~value
        ) in
      {xs_; result_}

    | (Some xs, None) ->
      let result_ = Some (
          xs
          |> Z.to_array
          |> Z.of_array ~index
        ) in
      update {xs_; result_} ~index ~value
    | (None, _) -> {xs_; result_}

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
