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
    point_map: Stencil.point_map;
  }

  let _OP_NAME: Stencil.stencil_id = "COPY"
  let _VAR_X: Stencil.op_arg = {op=_OP_NAME; arg="X"}
  let _VAR_Y: Stencil.op_arg = {op=_OP_NAME; arg="Y"}

  let make ~context ~names =
    let x_id = Hashtbl.find names _VAR_X in
    let x = Hashtbl.find context x_id in
    let _ = if ST.is_empty x then
        failwith @@ Printf.sprintf "Empty stencil found for variable '%s' in operation '%s'" _VAR_X.arg _VAR_X.op
      else ()
    in

    let y_id = Hashtbl.find names _VAR_Y in
    let y = Hashtbl.find context y_id in
    let _ = if ST.is_empty y then
        failwith @@ Printf.sprintf "Empty stencil found for variable '%s' in operation '%s'" _VAR_Y.arg _VAR_Y.op
      else ()
    in

    let _ = if not @@ ST.is_compatible x y then
        failwith @@ Printf.sprintf "Incompatible stencils '%s' and '%s in operation '%s'" _VAR_X.arg _VAR_Y.arg _OP_NAME
      else ()
    in
    let n = ST.elements x in
    let point_map = Hashtbl.create n in
    let f = fun _ px py ->
      Hashtbl.add point_map px py
    in
    let io: IO.t = {input=x; output=y} in
    let () = io.input |> ST.iter_zip io.output ~f in
    {io; point_map}

  let full_calc t =
    let f = fun dirty_output ->
      let copy_x_to_y = fun x _ py ->
        dirty_output |> ST.write ~p:py ~value:x
      in
      t.io.input |> ST.iter_zip dirty_output ~f:copy_x_to_y
    in

    let io = t.io |> IO.full_calc_with ~f in
    {t with io}

  let update t point =
    if not @@ Hashtbl.mem t.point_map point then
      t
    else
      (* NOTE f is invoked after checking points exist in stencil
       *  but we also check here that we have points in the point_map *)
      (* TODO could be done better - want to keep the point_map access
       * close to where we check the stencils *)
      let p = Hashtbl.find t.point_map point in
      let f = fun dirty_output ->
        let value = ST.read t.io.input ~p:point in
        ST.write dirty_output ~p ~value
      in
      let op_name = _OP_NAME in
      let io = t.io |> IO.update_with ~f ~pointX:point ~pointY:p ~op_name in
      {t with io}

end
