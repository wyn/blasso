(* NOTE
 *
 * only ever updating xs at one index location at a time
 * and also updating the result with the appropriate new calc value/values
 *
 *  If the result hasnt been set (=None) then do full calculation first
 *  and then update
 *
 * if any of the input/inputs are not set then also dont do anything yet *)

module Z = Zipper

(* X corresponds to the first coordinate, Y the second *)
type coord = | X | Y

module type Blas_data = sig
  type input
  type output
  type t

  val init: ?coord:coord -> xarr:float array -> t

  val result: t -> float array option

  val hash: t -> int

  val update: t -> index:int -> value:float -> coord:coord -> t

end

module One_dim = struct

  type array_view = Z.t

  type t = {
    xs_: array_view;
  }

  let of_zipper xs =
    let xs_=xs in
    {
      xs_;
    }

  let of_array xarr =
    xarr |> (Z.of_array ~index:0) |> of_zipper

  let input t = t.xs_ (* |> Option.map Z.to_array *)
  let output t = t.xs_

  let update ({xs_; _} as t) ~index ~value =
    let xs_at_i = xs_ |> Z.jump_to ~index in
    let x0 = Z.get xs_at_i in
    match x0 == value with
    | true -> t
    | false ->
      let xs_ = xs_at_i |> Z.set ~value in
      {
        xs_;
      }

end



module Copy_data = struct

  include One_dim

end


module Scale_data = struct

  type input = One_dim.t
  type output = One_dim.t

  type t = {
    xs_: input;
    result_: output;
    alpha_: float;
  }

  let link input ~alpha =
    let xs_ = input in
    let result_ = One_dim.input input |> Z.map ~f:(fun x -> x *. alpha) |> One_dim.of_zipper in
    {
      xs_;
      result_;
      alpha_=alpha;
    }


  let input t = t.xs_
  let output t = t.result_

  let update {xs_; result_; alpha_} ~index ~value =
    let xs_ = One_dim.input xs_
              |> Z.jump_to ~index
              |> Z.set ~value
              |> One_dim.of_zipper
    in
    let result_ = One_dim.input result_
                  |> Z.jump_to ~index
                  |> Z.set ~value:(value *. alpha_)
                  |> One_dim.of_zipper
    in
    {
      xs_;
      result_;
      alpha_;
    }

end


module Dot_data = struct

  type input = Z.t
  type output = float (* not really a float, its a list of sums of pointwise multiplies *)

  type t = {
    xs_: input option;
    ys_: input option;
    result_: output option;
  }

  let init ~xarr ~yarr = {
    xs_=xarr |> Option.map (Z.of_array ~index:0);
    ys_=yarr |> Option.map (Z.of_array ~index:0);
    result_=None;
  }

  let result t = t.result_

  let rec update ({xs_; ys_; result_} as t) ~index ~value ~coord =
    match (xs_, ys_, result_) with
    | (Some xs, Some ys, Some prev) ->
      let xs_at_i = xs |> Z.jump_to ~index in
      let x0 = xs_at_i |> Z.get in
      let ys_at_i = ys |> Z.jump_to ~index in
      let y0 = ys_at_i |> Z.get in
      let xs_, ys_, result_ = match coord with
        | X -> (
            Some (xs_at_i |> Z.set ~value),
            Some ys_at_i,
            Some (prev +. y0 *. (value -. x0))
          )
        | Y -> (
            Some xs_at_i,
            Some (ys_at_i |> Z.set ~value),
            Some (prev +. x0 *. (value -. y0))
          )
      in
      {xs_; ys_; result_}

    | (Some xs, Some ys, None) ->
      let xarr = xs |> Z.to_array in
      let yarr = ys |> Z.to_array in
      let result_ = Some (yarr
                          |> Array.map2 (fun x y -> x *. y) xarr
                          |> Array.fold_left (fun acc v -> acc +. v) 0.)
      in
      update {xs_; ys_; result_} ~index ~value ~coord

    | (None, _, _) | (_, None, _) -> t

end


(* module Swap_data = struct
 *
 *   type input = Z.t
 *
 *   type output = {
 *     oxs_: Z.t;
 *     oys_: Z.t;
 *   }
 *
 *   type t = {
 *     xs_: input option;
 *     ys_: input option;
 *     result_: output option;
 *   }
 *
 *   let init ~xarr ~yarr = {
 *     xs_=xarr |> Option.map (Z.of_array ~index:0);
 *     ys_=yarr |> Option.map (Z.of_array ~index:0);
 *     result_=None;
 *   }
 *
 *   let result t = t.result_
 *
 *   let rec update ({xs_; ys_; result_} as t) ~index ~value ~coord =
 *     match (xs_, ys_, result_) with
 *     | (Some xs, Some ys, Some prev) -> begin
 *         match coord with
 *         | X ->
 *           let xs_ = Some (xs
 *                           |> Z.jump_to ~index
 *                           |> Z.set ~value)
 *           in
 *           let ys_ = Some (ys |> Z.jump_to ~index) in
 *           let prev_oxs = prev.oxs_ |> Z.jump_to ~index in
 *           let prev_oys = prev.oys_
 *                          |> Z.jump_to ~index
 *                          |> Z.set ~value
 *           in
 *           let result_ = Some {
 *               oxs_=prev_oxs;
 *               oys_=prev_oys;
 *             } in
 *           {xs_; ys_; result_}
 *         | Y ->
 *           let xs_ = Some (xs |> Z.jump_to ~index) in
 *           let ys_ = Some (ys
 *                           |> Z.jump_to ~index
 *                           |> Z.set ~value)
 *           in
 *           let prev_oxs = prev.oxs_
 *                          |> Z.jump_to ~index
 *                          |> Z.set ~value
 *           in
 *           let prev_oys = prev.oys_ |> Z.jump_to ~index in
 *           let result_ = Some {
 *               oxs_=prev_oxs;
 *               oys_=prev_oys;
 *             } in
 *           {xs_; ys_; result_}
 *       end
 *
 *     | (Some xs, Some ys, None) ->
 *       let result_ = Some {
 *           oxs_=ys |> Z.to_array |> Z.of_array ~index;
 *           oys_=xs |> Z.to_array |> Z.of_array ~index;
 *         } in
 *       update {xs_; ys_; result_} ~index ~value ~coord
 *
 *     | (None, _, _) | (_, None, _) -> t
 *
 * end *)


type blas_expr =
  (* | Scalar of Scalar.t *)
  | Arr of One_dim.t
  (* | Copy of blas_expr *)
  | Scale of blas_expr
  | Dot of blas_expr * blas_expr
  (* | Swap of blas_expr * blas_expr *)


(* update : BEXPR -> index -> value -> coord -> BEXPR
 * when stencilling is done index/coord will be a stencil point *)

let rec update blas_expr ~index ~value =
  match blas_expr with
  (* | Scalar v -> Scalar.update v ~value *)
  | Arr data -> Arr (One_dim.update data ~index ~value)
  | Scale bexpr -> bexpr
  (* let xs = update bexpr ~index ~value ~coord in
   * Scale_data.update xs ~index ~value ~coord *)
  | Dot (bexprX, _bexprY) -> bexprX
(* let xs = update bexprX ~index ~value ~coord in
 * let ys = update bexprY ~index ~value ~coord in
 * Dot_data.update xs ys ~index ~value ~coord *)


(* rot/rotg - setup and doing
 * max index
 * sum abs
 * norm - sqrt (sum (xi**2)) /n ? -> sum (xi**2)
 * axpy
 *  *)

(* want to do stuff like
 *
 * let x = 1D(array...) in
 * let y = Copy(x) |> Scale(0.3) in
 * let z = Dot(x, y) # TODO update on y too
 *
 * let z_ = Update(z, 3, x=400.)
 * or
 * let z_ = Update(z, 2, y=-1.)
 *  *)
