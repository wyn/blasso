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
(* NOTE not sure we need this *)
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

  let hash {row; col} = Coord.(hash row + hash col)

end

type stencil_id = string

module type STENCIL = sig

  type t

  (* contructors - can make empty stencils (underlying data is garbage array), or direct from existing data *)
  val empty : nrows:int -> ncols:int -> orientation:axis -> t
  val empty_row : ncols:int -> t
  val empty_col : nrows:int -> t

  val make : data:(float array array) -> orientation:axis -> t
  val make_row : data:(float array) -> t
  val make_col : data:(float array) -> t
  val make_scalar : data:float -> t

  val reset_data : t -> data:(float array array) -> t
  val read : t -> p:Point.t -> float
  val read_first : t -> float (* NOTE for scalars, dont need to know where the scalar is - will be something like [| 00000 1 0000...000 |] *)
  val write : t -> p:Point.t -> value:float -> unit
  val write_first : t -> value:float -> unit (* NOTE for scalars, dont need to know where the scalar is - will be something like [| 00000 1 0000...000 |] *)

  val hash : t -> int
  val (=) : t -> t -> bool
  val is_compatible : t -> t -> bool

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
  (* NOTE not sure we need parent and data ids *)
  val id : t -> stencil_id
  val children_ids : t -> stencil_id list
  val parent_id : t -> stencil_id
  val data_id : t -> stencil_id

  val rows: t -> int
  val cols: t -> int
  val elements: t -> int
  (* what is my state with respect to an output *)
  val state : t -> wrt:stencil_id -> state
  val mark_dirty : t -> t
  (* mark me clean with respect to an output *)
  val mark_clean : t -> wrt:stencil_id -> t

  (* these getters return stencils that allow one to get at a value *)
  val slice : t -> ?from:Point.t -> ?nrows:int -> ?ncols:int -> t
  val row : t -> r:int -> t
  val col : t -> c:int -> t
  val get : t -> p:Point.t -> t
  val mem : t -> p:Point.t -> bool

  val is_empty : t -> bool
  val is_scalar : t -> bool
  val is_row : t -> bool
  val is_col : t -> bool

  val iter : t -> f:(Point.t -> unit) -> unit
  val iter_zip : t -> t -> f:(Point.t -> Point.t -> unit) -> unit

end


type arg = {this: stencil_id; arg_name: string;}
type 'stencil context = (arg, 'stencil) Hashtbl.t
type point_map = (Point.t, Point.t) Hashtbl.t

module type BLAS_OP = sig

  type stencil

  type t

  (* context is the stencil ID -> stencil map,
   * this is some sort of id for the operation at that point in the stack
   * e.g.
   * let alpha = ST.make_scalar 0.2
   * let st0 = ...
   * let res = Scale X=st0 alpha=alpha
   *
   * ```context``` is { {res.id, 'X'}:st0 stencil; {res.id, 'alpha'}:alpha stencil; ... }
   * ```this``` is res.id
   *
   *  *)
  val make : context:stencil context -> this:stencil_id -> t

  val full_calc : t -> t

  val update : t -> Point.t -> t

end


module IO_ (ST: STENCIL) = struct
  type t = {
    input: ST.t;
    output: ST.t;
    point_map: point_map;
    op_name: string;
  }

  let make ~input ~output ~op_name =
    let _ = if not @@ ST.is_compatible input output then
        failwith @@ Printf.sprintf
          "Incompatible stencils '%s' and '%s in operation '%s'"
          (ST.id input) (ST.id output) op_name
      else ()
    in
    let n = ST.elements input in
    let point_map = Hashtbl.create n in
    let f = fun px py ->
      Hashtbl.add point_map px py
    in
    let () = input |> ST.iter_zip output ~f in
    {input; output; point_map; op_name}


  let _dirty_wrapper ({input; output; _} as t) ~f =
    let dirty_output = ST.mark_dirty output in
    let () = f dirty_output in
    let clean_input = ST.mark_clean input ~wrt:(ST.id dirty_output) in
    {t with input=clean_input; output=dirty_output}

  let full_calc_with ({input; output; _} as t) ~f =
    match ST.state input ~wrt:(ST.id output) with
    | CLEAN-> t
    | DIRTY | NOT_INITIALISED ->
      let dirty_output = ST.mark_dirty output in
      let () = f input dirty_output in
      let clean_input = ST.mark_clean input ~wrt:(ST.id dirty_output) in
      {t with input=clean_input; output=dirty_output}

  let update_with ({input; output; point_map; op_name} as t) ~f ~pointX =
    let has_input = ST.mem input ~p:pointX in
    let pY = Hashtbl.find_opt point_map pointX in
    let has_output = pY
                     |> Option.map (fun p -> ST.mem output ~p)
                     |> Option.value ~default:false in
    match (has_input, has_output, pY) with
    | (true, true, Some pointY) -> begin
        (* if input is DIRTY with respect to our output
         * then we need to update output and mark whole output as DIRTY
         * and also set input with respect to this output as clean
         * *)
        match ST.state input ~wrt:(ST.id output) with
        | CLEAN -> t
        | DIRTY ->
          let dirty_output = ST.mark_dirty output in
          let () = f input dirty_output pointX pointY in
          let clean_input = ST.mark_clean input ~wrt:(ST.id dirty_output) in
          {t with input=clean_input; output=dirty_output}
        | NOT_INITIALISED ->
          failwith @@ Printf.sprintf
            "Not initialised - '%s' cannot continue" op_name
      end

    (* not taking part in this update *)
    | _ -> t

end
