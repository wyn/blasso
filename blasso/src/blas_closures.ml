module Coord = struct
  type t = | X | Y

  let first = X
  let second = Y

end

module Stencil = struct
  type t = {
    index: int;
    coord: Coord.t
  }

  let index t = t.index
  let coord t = t.coord

end


module Blas01 = struct

  type t = float array

  let scale t ~alpha =
    let xs = t |> Array.map (fun x -> alpha *. x) in
    fun ~index ~value ->
      let () = xs.(index) <- alpha *. value in
      xs

  let dot t yarr =
    let result = [|
      t
      |> Array.map2 (fun x y -> x *. y) yarr
      |> Array.fold_left (fun acc v -> acc +. v) 0.
    |] in
    fun ~index ~value ->
      let x0 = t.(index) in
      let y0 = yarr.(index) in
      let prev = result.(0) in
      let () = result.(0) <- (prev +. y0 *. (value -. x0)) in
      result


end
