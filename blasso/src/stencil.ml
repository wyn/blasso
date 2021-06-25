(* is the stencil pointing to underlying data that needs to be updated
 * stencils start off uninitialised,
 * then underlying data is fully populated and they become clean,
 * then the underlying data is updated in one place and they become dirty
 *  *)
type state = | CLEAN | DIRTY | NOT_INITIALISED

(* a blas op will read from one array and write to another (possibly same) array
 * using stencils as the view onto the arrays
 * WRITE implies READ too
 * writes are only done on downstream arrays
 * dirty reads can be done on downstream arrays (this is the 'cache')
 * uptream reads have to be done with clean stencils
 * upstream writes are not allowed
 * if a stencil is dirty it will update itself first *)
type role = | READ | WRITE

type axis = | ROW | COL


module Coord = struct

  type t = {
    axis: axis;
    index: int;
  }

  let r ~i = {axis=ROW; index=i}
  let c ~j = {axis=COL; index=j}

  let hash {axis; index} = Hashtbl.(hash axis + hash index)

end


module Point = struct

  type t = {
    row: Coord.t;
    col: Coord.t;
  }

  let p ~i ~j = {
    row=Coord.r ~i;
    col=Coord.c ~j;
  }

  let hash {row; col} = Coord.hash row + Coord.hash col

end


module type STENCIL = sig

  type t

  (* contructors - can make empty stencils (underlying data is garbage array), or direct from existing data *)
  val empty : nrows:int -> ncols:int -> orientation:axis -> t
  val empty_row : ncols:int -> t
  val empty_col : nrows:int -> t

  val make : data:(float array array) -> orientation:axis -> t
  val make_row : data:(float array) -> t
  val make_col : data:(float array) -> t

  val reset_data : t -> data:(float array array) -> t
  val read : t -> p:Point.t -> float
  val write : t -> p:Point.t -> value:float -> unit

  (* something will need to manage stencils' IDs and also
   *  - a parent - the immediate upstream obj it is a view of
   *  - the id of the data it ultimately is a view of
   * e.g.
   *
   * let st0 = data |> make ~orientation:ROW |> slice ~from ~nrows ~ncols |> row ~r:3
   * let st1 = st0 |> get ~p
   *
   * then for st0:
   *  id refers to id of st0,
   *  parent_id is from the slice,
   *  and data_id is from the data
   *
   * and for st1:
   *  id refers to id of st1
   *  parent_id is id of st0
   *  data_id is still id of data
   *
   *  *)
  val hash : t -> int
  val id : t -> int
  val parent_id : t -> int
  val data_id : t -> int
  val (=) : t -> t -> bool
  val is_compatible : t -> t -> bool

  val rows: t -> int
  val cols: t -> int
  val elements: t -> int
  val state : t -> wrt:int -> state
  val mark_dirty : t -> t
  val mark_clean : t -> wrt:int -> t

  (* these getters return stencils that allow one to get at a value *)
  val slice : t -> ?from:Point.t -> ?nrows:int -> ?ncols:int -> t
  val row : t -> r:int -> t
  val col : t -> c:int -> t
  val get : t -> p:Point.t -> t
  val mem : t -> p:Point.t -> bool

  val iter : t -> f:(float -> float) -> output:t -> unit

end


module type BLAS_OP = sig

  type t

  val full_calc : t -> t

  val update : t -> Point.t -> t

end


module Scale (ST: STENCIL): BLAS_OP = struct

  type t = {
    input: ST.t;
    output: ST.t;
    alpha: float;
  }

  let full_calc t =
    match ST.state t.input ~wrt:(ST.id t.output) with
    | CLEAN-> t
    | DIRTY | NOT_INITIALISED ->
      let dirty_output = ST.mark_dirty t.output in
      let () = t.input |> ST.iter ~f:(fun x -> t.alpha  *. x) ~output:dirty_output in
      let clean_input = ST.mark_clean t.input ~wrt:(ST.id dirty_output) in
      {t with input=clean_input; output=dirty_output}

  let update t point =
    let has_input = ST.mem t.input ~p:point in
    let has_output = ST.mem t.output ~p:point in
    match (has_input, has_output) with
    | (true, true) -> begin
        let dirty_output = ST.mark_dirty t.output in
        (* if input is DIRTY with respect to our output
         * then we need to update output and mark whole output as DIRTY
         * and also set input with respect to this output as clean
         * *)
        match ST.state t.input ~wrt:(ST.id dirty_output) with
        | CLEAN -> t
        | DIRTY ->
          let value = ST.read t.input ~p:point in
          let new_scaled_value = t.alpha *. value in
          let () = ST.write dirty_output ~p:point ~value:new_scaled_value in
          let clean_input = ST.mark_clean t.input ~wrt:(ST.id dirty_output) in
          {t with input=clean_input; output=dirty_output}
        | NOT_INITIALISED -> failwith "Not initialised - Scale cannot continue"
      end

    (* not taking part in this update*)
    | _ -> { t with input=ST.mark_clean t.input ~wrt:(ST.id t.output) }

end
