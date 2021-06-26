(* DCOPY equivalent reads/writes to different arrays
 * reads from x and writes to y
 * DCOPY(n, xs, incx, ys, incy) -> ()
 *  *)
module type STENCIL = Stencil.STENCIL
module type BLAS_OP = Stencil.BLAS_OP

module Copy (ST: STENCIL): (BLAS_OP with type stencil := ST.t) = struct

  module IO = struct include Stencil.IO_(ST) end

  type t = {
    io: IO.t;
  }

  let _OP_NAME: Stencil.stencil_id = "COPY"
  let _VAR_X: Stencil.op_arg = {op=_OP_NAME; arg="X"}
  let _VAR_Y: Stencil.op_arg = {op=_OP_NAME; arg="Y"}

  let make ~context ~names =
    let x_id = Hashtbl.find names _VAR_X in
    let x = Hashtbl.find context x_id in
    let _ = if ST.is_empty x then
        failwith @@ Printf.sprintf
          "Empty stencil found for variable '%s' in operation '%s'"
          _VAR_X.arg _VAR_X.op
      else ()
    in

    let y_id = Hashtbl.find names _VAR_Y in
    let y = Hashtbl.find context y_id in
    let _ = if ST.is_empty y then
        failwith @@ Printf.sprintf
          "Empty stencil found for variable '%s' in operation '%s'"
          _VAR_Y.arg _VAR_Y.op
      else ()
    in

    let io = IO.make ~input:x ~output:y ~op_name:_OP_NAME in
    {io; }

  let _copy_x_to_y input output pointX pointY =
    let value = ST.read input ~p:pointX in
    ST.write output ~p:pointY ~value

  let full_calc t =
    let f = fun input output ->
      input |> ST.iter_zip output ~f:(_copy_x_to_y input output)
    in
    let io = t.io |> IO.full_calc_with ~f in
    {io; }

  let update t point =
    let io = t.io |> IO.update_with ~f:_copy_x_to_y ~pointX:point in
    {io; }

end
