(* NOTE
 *
 * only ever updating xs at one index location at a time
 * and also updating the result with the appropriate new calc value/values
 *
 *  If the result hasnt been set (=None) then do full calculation first
 *  and then update
 *
 * if any of the input/inputs are not set then also dont do anything yet *)

type coord = | X | Y | Z

module Dot_data = struct

  type _el = float Current_incr.var
  type _arr = float array (* Current_incr.var *)

  type t = {
    xs_: _arr;
    ys_: _arr;
    result_: _el;
  }

  let _dot xarr yarr =
    yarr
    |> Array.map2 (fun x y -> x *. y) xarr
    |> Array.fold_left (fun acc v -> acc +. v) 0.

  let init ~xarr ~yarr = {
    xs_ = xarr;
    ys_ = yarr;
    result_ = _dot xarr yarr |> Current_incr.var;
  }

  (* TODO figure out how to safely propagate changes then make new t *)
  let update {xs_; ys_; result_} ~index ~value ~coord =
    Current_incr.(
      of_cc begin
        read (of_var result_) @@ fun prev ->
        let xi = xs_.(index) in
        let yi = ys_.(index) in
        let result = match coord with
          | X ->
            let _ = xs_.(index) <- value in
            prev +. yi *. (value -. xi)
          | Y ->
            let _ = ys_.(index) <- value in
            prev +. xi *. (value -. yi)
          | Z -> prev
        in
        write result
      end
    )

end


(* let scale xs ~alpha ~index ~value ~coord =
 *   () *)
