(* DSCAL equivalent reads/writes to same array
 * DSCAL(n, alpha, xs, incx) -> ()
 *  *)
module type STENCIL = Stencil.STENCIL
module type BLAS_OP = Stencil.BLAS_OP

module Scale (ST: STENCIL): (BLAS_OP with type stencil := ST.t) = struct

  module IO = struct include Stencil.IO_(ST) end

  type t = {
    io: IO.t;
    alpha: ST.t;
  }

  let _OP_NAME: Stencil.stencil_id = "SCALE"
  let _VAR_X this : Stencil.arg = {this; arg_name="X"}
  let _VAR_ALPHA this : Stencil.arg = {this; arg_name="ALPHA"}

  let make ~context ~this =
    let var_x = _VAR_X this in
    let x = Hashtbl.find context var_x in
    let _ = if ST.is_empty x then
        failwith @@ Printf.sprintf
          "Empty stencil found for variable '%s' in operation '%s'"
          var_x.arg_name _OP_NAME
      else ()
    in

    let var_alpha = _VAR_ALPHA this in
    let alpha = Hashtbl.find context var_alpha in
    let _ = if ST.is_empty alpha then
        failwith @@ Printf.sprintf
          "Empty stencil found for variable '%s' in operation '%s'"
          var_alpha.arg_name _OP_NAME
      else ()
    in
    let _ = if not @@ ST.is_scalar alpha then
        failwith @@ Printf.sprintf
          "Expected scalar stencil for variable '%s' in operation '%s'"
          var_alpha.arg_name _OP_NAME
      else ()
    in

    (* NOTE input and output are both x stencil *)
    let io = IO.make ~input:x ~output:x ~op_name:_OP_NAME in
    {io; alpha}

  let _scale_by_alpha ~alpha input output pointX pointY =
    let x = ST.read input ~p:pointX in
    ST.write output ~p:pointY ~value:(alpha *. x)

  let full_calc t =
    (* know that alpha is a scalar so read_first *)
    let alpha = ST.read_first t.alpha in
    let f = fun input output ->
      let scale_by_alpha = _scale_by_alpha ~alpha input output in
      input |> ST.iter_zip output ~f:scale_by_alpha
    in
    let io = t.io |> IO.full_calc_with ~f in
    {t with io}

  let update t point =
    (* know that alpha is a scalar so read_first *)
    let alpha = ST.read_first t.alpha in
    let f = _scale_by_alpha ~alpha in
    let io = t.io |> IO.update_with ~f ~pointX:point in
    {t with io}

end
