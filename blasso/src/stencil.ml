(* is the stencil pointing to underlying data that needs to be updated
 * stencils start off uninitialised,
 * then underlying data is fully populated and they become clean,
 * then the underlying data is updated in one place and they become dirty
 *  *)
type state = | CLEAN | DIRTY | NOT_INITIALISED

type axis = | ROW | COL

(* a blas op will read from one array and write to another (possibly same) array
 * using stencils as the view onto the arrays
 * WRITE implies READ too
 * writes are only done on downstream arrays
 * dirty reads can be done on downstream arrays (this is the 'cache')
 * uptream reads have to be done with clean stencils
 * upstream writes are not allowed
 * if a stencil is dirty it will update itself first *)
type role = | READ | WRITE

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

  val make: rows:int -> cols:int -> orientation:axis -> t
  val make_row: cols:int -> t
  val make_col: rows:int -> t

  (* something will need to manage stencils' IDs and also
   *  - a parent - the immediate upstream obj it is a view of
   *  - the id of the data it ultimately is a view of *)
  val hash : t -> int
  val state : t -> state

  val is_compatible : t -> t -> bool
  val (=) : t -> t -> bool

  val rows: t -> int
  val cols: t -> int
  val elements: t -> int

  (* these getters return stencils that allow one to get at a value *)
  val slice : t -> ?from:Point.t -> ?nrows:int -> ?ncols:int -> t
  val row : t -> int -> t
  val col : t -> int -> t
  val get : t -> Point.t -> t
  val mem : t -> Point.t -> bool

end
