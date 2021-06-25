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
  let _VAR_X: Stencil.op_arg = {op=_OP_NAME; arg="X"}
  let _VAR_ALPHA: Stencil.op_arg = {op=_OP_NAME; arg="ALPHA"}

  let make ~context ~names =
    let x_id = Hashtbl.find names _VAR_X in
    let x = Hashtbl.find context x_id in
    let _ = if ST.is_empty x then
        failwith @@ Printf.sprintf "Empty stencil found for variable '%s' in operation '%s'" _VAR_X.arg _VAR_X.op
      else ()
    in

    let alpha_id = Hashtbl.find names _VAR_ALPHA in
    let alpha = Hashtbl.find context alpha_id in
    let _ = if ST.is_empty alpha then
        failwith @@ Printf.sprintf "Empty stencil found for variable '%s' in operation '%s'" _VAR_ALPHA.arg _VAR_ALPHA.op
      else ()
    in
    let _ = if not @@ ST.is_scalar alpha then
        failwith @@ Printf.sprintf "Expected scalar stencil for variable '%s' in operation '%s'" _VAR_ALPHA.arg _VAR_ALPHA.op
      else ()
    in

    let io: IO.t = {input=x; output=x} in

    {io; alpha}

  let full_calc t =
    match ST.state t.io.input ~wrt:(ST.id t.io.output) with
    | CLEAN-> t
    | DIRTY | NOT_INITIALISED ->
      let io = IO.dirty_wrapper t.io ~f:(
          fun dirty_output ->
            let alpha = ST.read_first t.alpha in
            let scale_by_alpha = fun x p -> dirty_output |> ST.write ~p ~value:(alpha  *. x) in
            t.io.input |> ST.iter ~f:scale_by_alpha
        ) in
      {t with io}

  let update t point =
    let has_input = ST.mem t.io.input ~p:point in
    let has_output = ST.mem t.io.output ~p:point in
    match (has_input, has_output) with
    | (true, true) -> begin
        (* if input is DIRTY with respect to our output
         * then we need to update output and mark whole output as DIRTY
         * and also set input with respect to this output as clean
         * *)
        match ST.state t.io.input ~wrt:(ST.id t.io.output) with
        | CLEAN -> t
        | DIRTY ->
          let io = IO.dirty_wrapper t.io ~f:(
              fun dirty_output ->
                let value = ST.read t.io.input ~p:point in
                let alpha = ST.read_first t.alpha in
                let new_scaled_value = alpha *. value in
                ST.write dirty_output ~p:point ~value:new_scaled_value
            ) in
          {t with io}
        | NOT_INITIALISED ->
          failwith @@ Printf.sprintf "Not initialised - '%s' cannot continue" _OP_NAME
      end

    (* not taking part in this update*)
    | _ -> t
    (* let io = {t.io with input=ST.mark_clean t.io.input ~wrt:(ST.id t.io.output)} in
     * {t with io} *)

end
