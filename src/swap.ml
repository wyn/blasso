type n = int
  (* | N of int *)
type incx = int
  (* | IncX of int *)
type incy = int
  (* | IncY of int *)

let dswap
      (n : n)
      (dx : float array)
      (incx : incx)
      (dy : float array)
      (incy : incy)
  =  
  if n > 0 then
    let dtemp = ref 0. in
    let ix = ref (if incx < 0 then (1-n)*incx else 0) in
    let iy = ref (if incy < 0 then (1-n)*incy else 0) in
    Printf.printf "ix %d, iy %d, dtemp %f\n" !ix !iy !dtemp;
    for i = 0 to n-1 do
      Printf.printf "%d: ix %d, iy %d, dtemp %f\n" i !ix !iy !dtemp;
      dtemp := dx.(!ix);
      dx.(!ix) <- dy.(!iy);
      dy.(!iy) <- !dtemp;
      ix := !ix + incx;
      iy := !iy + incy;
      Printf.printf "%d: ix %d, iy %d, dtemp %f\n" i !ix !iy !dtemp;
    done
