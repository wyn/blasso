
let dswap ~n ~xs ~incx ~ys ~incy = 
  if n > 0 then
    let dtemp = ref 0. in
    let ix = ref (if incx < 0 then (1-n)*incx else 0) in
    let iy = ref (if incy < 0 then (1-n)*incy else 0) in
    Printf.printf "ix %d, iy %d, dtemp %f\n" !ix !iy !dtemp;
    for i = 0 to n-1 do
      Printf.printf "%d: ix %d, iy %d, dtemp %f\n" i !ix !iy !dtemp;
      dtemp := xs.(!ix);
      xs.(!ix) <- ys.(!iy);
      ys.(!iy) <- !dtemp;
      ix := !ix + incx;
      iy := !iy + incy;
      Printf.printf "%d: ix %d, iy %d, dtemp %f\n" i !ix !iy !dtemp;
    done


let dscal ~n ~alpha ~xs ~incx =
  if n > 0 && incx > 0 then
    let ix = ref 0 in
    for i = 0 to n-1 do
      xs.(!ix) <- alpha *. xs.(!ix);
      ix := !ix + incx;
  done

let drotg ~alpha ~beta ~c ~s =
  let abs_alpha = abs_float !alpha in
  let abs_beta = abs_float !beta in
  let roe = if (abs_alpha > abs_beta) then !alpha else !beta in
  let scale = abs_alpha +. abs_beta in
  let r = ref 0. in
  let z = ref 0. in
  let scaling () = 
    if scale == 0. then (
      c := 1.;
      s := 0.;
    )
    else (
      r := scale *. sqrt ( (!alpha /. scale)**2. +. (!beta /. scale)**2. );
      r := (copysign 1. roe) ** (!r);
      c := !alpha /. !r;
      s := !beta /. !r;
      z := 1.;
      if abs_alpha > abs_beta then z := !s;
      if abs_beta >= abs_alpha && !c != 0. then z := 1. /. !c;
    )
  in (
      scaling();
      alpha := !r;
      beta := !z;
    )

let drot ~n ~xs ~incx ~ys ~incy ~c ~s = 
  if n > 0 then
    let dtemp = ref 0. in
    let ix = ref (if incx < 0 then (1-n)*incx else 0) in
    let iy = ref (if incy < 0 then (1-n)*incy else 0) in
    Printf.printf "ix %d, iy %d, dtemp %f\n" !ix !iy !dtemp;
    for i = 0 to n-1 do
      Printf.printf "%d: ix %d, iy %d, dtemp %f\n" i !ix !iy !dtemp;
      dtemp := c *. xs.(!ix) +. s *. ys.(!iy);
      ys.(!iy) <- c *. ys.(!iy) -. s *. xs.(!ix);
      xs.(!ix) <- !dtemp;
      ix := !ix + incx;
      iy := !iy + incy;
      Printf.printf "%d: ix %d, iy %d, dtemp %f\n" i !ix !iy !dtemp;
    done
