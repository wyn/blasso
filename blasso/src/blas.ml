(* NOTE
 *
 * only ever updating xs at one index location at a time
 * and also updating the result with the appropriate new calc value/values
 *
 *  If the result hasnt been set (=None) then do full calculation first
 *  and then update
 *
 * if any of the input/inputs are not set then also dont do anything yet
 *
 * Looks like we might want to split up into blas level 1,2,3
 * and then within level 1,
 * ops with type sigs:
 * vector -> scalar e.g. norm, abs sum, max index
 * vector -> vector e.g. scale alpha
 * vector, vector -> scalar e.g. dot,
 * vector, vector -> vector e.g. rot, axpy, copy
 * vector, vector -> vector, vector e.g. swap
 *  *)
(* X corresponds to the first coordinate, Y the second *)
type coord = | X | Y
type stencil = {coord:coord;
                index:int;
                value:float;
               }

type 'scalar vec_to_scalar = float array -> 'scalar
type vec_to_vec = float array -> float array
type 'scalar vec2_to_scalar = float array -> float array -> 'scalar
type vec2_to_vec = float array -> float array -> float array
type vec2_to_vec2 = float array -> float array -> float array -> float array


module Abs_sum = struct
  type calc = float vec_to_scalar
  type t = {calc: calc;
            cache: float option ref;
            coord: coord;
           }

  let dasum xs ~n ~incx =
    let dtemp = ref 0. in
    let helper () =
      if n > 0 && incx > 0 then (
        let ix = ref 0 in
        for _i = 0 to n-1 do
          dtemp := !dtemp +. abs_float xs.(!ix);
          ix := !ix + incx;
        done
      ) in (
      helper();
      !dtemp
    )

  let make ?coord:(coord=X) ?incx:(incx=1) n =
    let calc = fun xs -> dasum xs ~n ~incx in
    let cache = ref None in
    {
      calc;
      cache;
      coord;
    }

  let update_with t ~calc = {calc; cache=t.cache; coord=t.coord}

  let current t = !(t.cache)

end

(* NOTE usage:
 *  Abs_sum.make ~coord:X 10
 *  |> Abs_sum_updater.update ~stencil:{X 3 32.0}
 *  |> Ans_sum_updater.update ~stencil:{Y 5 0.0}
 *
 *  *)

module Abs_sum_updater = struct

  type t = Abs_sum.t

  let dasum_updater (abs_sum:Abs_sum.t) {coord; index; value} =
    fun xs -> begin
        let current = match Abs_sum.current abs_sum with
          | Some v -> v
          | None -> abs_sum.calc xs (* NOTE could hold off calculating this until coord==coord ? *)
        in
        match (coord == abs_sum.coord) with
        | false -> current (* this array hasnt changed *)
        | true ->
          (* NOTE probably can just check abs x0 == abs value *)
          let x0 = xs.(index) in
          match x0 == value with
          | true -> current
          | false ->
            (* fast update of result and cache *)
            let new_val = current -. abs_float x0 +. abs_float value in
            let () = abs_sum.cache := Some new_val in
            new_val
      end

  let update t ~stencil =
    let calc = dasum_updater t stencil in
    t |> Abs_sum.update_with ~calc


end

let r1, r2 =
  let xs = [|1.; -2.; 3.;|] in
  let u0 = Abs_sum.make 3 in
  let u1 =
    let stencil = {coord=X;
                   index=1;
                   value=30.0} in
    u0 |> Abs_sum_updater.update ~stencil
  in u0.calc xs, u1.calc xs
;;

(* module Z = Zipper
 *
 *
 * module type Blas_data = sig
 *   type input
 *   type output
 *   type t
 *
 *   val init: ?coord:coord -> xarr:float array -> t
 *
 *   val result: t -> float array option
 *
 *   val hash: t -> int
 *
 *   val update: t -> index:int -> value:float -> coord:coord -> t
 *
 * end
 *
 * module One_dim = struct
 *
 *   type array_view = Z.t
 *
 *   type t = {
 *     xs_: array_view;
 *   }
 *
 *   let of_zipper xs =
 *     let xs_=xs in
 *     {
 *       xs_;
 *     }
 *
 *   let of_array xarr =
 *     xarr |> (Z.of_array ~index:0) |> of_zipper
 *
 *   let input t = t.xs_ (\* |> Option.map Z.to_array *\)
 *   let output t = t.xs_
 *
 *   let update ({xs_; _} as t) ~index ~value =
 *     let xs_at_i = xs_ |> Z.jump_to ~index in
 *     let x0 = Z.get xs_at_i in
 *     match x0 == value with
 *     | true -> t
 *     | false ->
 *       let xs_ = xs_at_i |> Z.set ~value in
 *       {
 *         xs_;
 *       }
 *
 * end
 *
 *
 *
 * module Copy_data = struct
 *
 *   include One_dim
 *
 * end
 *
 *
 * module Scale_data = struct
 *
 *   type input = One_dim.t
 *   type output = One_dim.t
 *
 *   type t = {
 *     xs_: input;
 *     result_: output;
 *     alpha_: float;
 *   }
 *
 *   let link input ~alpha =
 *     let xs_ = input in
 *     let result_ = One_dim.input input
 *                   |> Z.map ~f:(fun x -> x *. alpha)
 *                   |> One_dim.of_zipper in
 *     {
 *       xs_;
 *       result_;
 *       alpha_=alpha;
 *     }
 *
 *
 *   let input t = t.xs_
 *   let output t = t.result_
 *
 *   let update {xs_; result_; alpha_} ~index ~value =
 *     let xs_ = One_dim.input xs_
 *               |> Z.jump_to ~index
 *               |> Z.set ~value
 *               |> One_dim.of_zipper
 *     in
 *     let result_ = One_dim.input result_
 *                   |> Z.jump_to ~index
 *                   |> Z.set ~value:(value *. alpha_)
 *                   |> One_dim.of_zipper
 *     in
 *     {
 *       xs_;
 *       result_;
 *       alpha_;
 *     }
 *
 * end
 *
 *
 * module Dot_data = struct
 *
 *   type input = Z.t
 *   type output = float (\* not really a float, its a list of sums of pointwise multiplies *\)
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
 *     | (Some xs, Some ys, Some prev) ->
 *       let xs_at_i = xs |> Z.jump_to ~index in
 *       let x0 = xs_at_i |> Z.get in
 *       let ys_at_i = ys |> Z.jump_to ~index in
 *       let y0 = ys_at_i |> Z.get in
 *       let xs_, ys_, result_ = match coord with
 *         | X -> (
 *             Some (xs_at_i |> Z.set ~value),
 *             Some ys_at_i,
 *             Some (prev +. y0 *. (value -. x0))
 *           )
 *         | Y -> (
 *             Some xs_at_i,
 *             Some (ys_at_i |> Z.set ~value),
 *             Some (prev +. x0 *. (value -. y0))
 *           )
 *       in
 *       {xs_; ys_; result_}
 *
 *     | (Some xs, Some ys, None) ->
 *       let xarr = xs |> Z.to_array in
 *       let yarr = ys |> Z.to_array in
 *       let result_ = Some (yarr
 *                           |> Array.map2 (fun x y -> x *. y) xarr
 *                           |> Array.fold_left (fun acc v -> acc +. v) 0.)
 *       in
 *       update {xs_; ys_; result_} ~index ~value ~coord
 *
 *     | (None, _, _) | (_, None, _) -> t
 *
 * end
 *
 *
 * (\* module Swap_data = struct
 *  *
 *  *   type input = Z.t
 *  *
 *  *   type output = {
 *  *     oxs_: Z.t;
 *  *     oys_: Z.t;
 *  *   }
 *  *
 *  *   type t = {
 *  *     xs_: input option;
 *  *     ys_: input option;
 *  *     result_: output option;
 *  *   }
 *  *
 *  *   let init ~xarr ~yarr = {
 *  *     xs_=xarr |> Option.map (Z.of_array ~index:0);
 *  *     ys_=yarr |> Option.map (Z.of_array ~index:0);
 *  *     result_=None;
 *  *   }
 *  *
 *  *   let result t = t.result_
 *  *
 *  *   let rec update ({xs_; ys_; result_} as t) ~index ~value ~coord =
 *  *     match (xs_, ys_, result_) with
 *  *     | (Some xs, Some ys, Some prev) -> begin
 *  *         match coord with
 *  *         | X ->
 *  *           let xs_ = Some (xs
 *  *                           |> Z.jump_to ~index
 *  *                           |> Z.set ~value)
 *  *           in
 *  *           let ys_ = Some (ys |> Z.jump_to ~index) in
 *  *           let prev_oxs = prev.oxs_ |> Z.jump_to ~index in
 *  *           let prev_oys = prev.oys_
 *  *                          |> Z.jump_to ~index
 *  *                          |> Z.set ~value
 *  *           in
 *  *           let result_ = Some {
 *  *               oxs_=prev_oxs;
 *  *               oys_=prev_oys;
 *  *             } in
 *  *           {xs_; ys_; result_}
 *  *         | Y ->
 *  *           let xs_ = Some (xs |> Z.jump_to ~index) in
 *  *           let ys_ = Some (ys
 *  *                           |> Z.jump_to ~index
 *  *                           |> Z.set ~value)
 *  *           in
 *  *           let prev_oxs = prev.oxs_
 *  *                          |> Z.jump_to ~index
 *  *                          |> Z.set ~value
 *  *           in
 *  *           let prev_oys = prev.oys_ |> Z.jump_to ~index in
 *  *           let result_ = Some {
 *  *               oxs_=prev_oxs;
 *  *               oys_=prev_oys;
 *  *             } in
 *  *           {xs_; ys_; result_}
 *  *       end
 *  *
 *  *     | (Some xs, Some ys, None) ->
 *  *       let result_ = Some {
 *  *           oxs_=ys |> Z.to_array |> Z.of_array ~index;
 *  *           oys_=xs |> Z.to_array |> Z.of_array ~index;
 *  *         } in
 *  *       update {xs_; ys_; result_} ~index ~value ~coord
 *  *
 *  *     | (None, _, _) | (_, None, _) -> t
 *  *
 *  * end *\)
 *
 * let xs = [||] |> One_dim.of_array |> Scale_data.link ~alpha:0.1 ;;
 *
 * type blas_expr =
 *   (\* | Scalar of Scalar.t *\)
 *   | Arr of One_dim.t
 *   (\* | Copy of blas_expr *\)
 *   | Scale of blas_expr * float
 *   (\* | Dot of blas_expr * blas_expr *\)
 * (\* | Swap of blas_expr * blas_expr *\)
 *
 *
 * (\* update : BEXPR -> index -> value -> coord -> BEXPR
 *  * when stencilling is done index/coord will be a stencil point *\)
 *
 * let rec update blas_expr ~index ~value =
 *   match blas_expr with
 *   (\* | Scalar v -> Scalar.update v ~value *\)
 *   | Arr data -> One_dim.update data ~index ~value
 *   | Scale (bexpr, alpha) ->
 *     let xs = update bexpr ~index ~value in
 *     let result = xs in
 *     let ys = Scale_data.update {xs_=xs; result_=result; alpha_=alpha} ~index ~value in
 *     Scale_data.output ys
 * (\* | Dot (bexprX, _bexprY) -> bexprX *\)
 * (\* let xs = update bexprX ~index ~value ~coord in
 *  * let ys = update bexprY ~index ~value ~coord in
 *  * Dot_data.update xs ys ~index ~value ~coord *\)
 *
 *
 * (\* rot/rotg - setup and doing
 *  * max index
 *  * sum abs
 *  * norm - sqrt (sum (xi**2)) /n ? -> sum (xi**2)
 *  * axpy
 *  *  *\)
 *
 * (\* want to do stuff like
 *  *
 *  * let x = 1D(array...) in
 *  * let y = Copy(x) |> Scale(0.3) in
 *  * let z = Dot(x, y) # TODO update on y too
 *  *
 *  * let z_ = Update(z, 3, x=400.)
 *  * or
 *  * let z_ = Update(z, 2, y=-1.)
 *  *  *\) *)
