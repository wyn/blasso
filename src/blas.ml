module Blas1 : sig
  
  val drotg : alpha:float ref -> beta:float ref ->
              c:float ref -> s:float ref ->
              unit

  (* val drotmg : alpha:float ref -> beta:float ref ->
   *              c:float ref -> s:float ref ->
   *              unit
   *)
    
  val drot : n:int ->
             xs:float array -> incx:int ->
             ys:float array -> incy:int ->
             c:float -> s:float ->
             unit

  (* val drotm : n:int ->
   *             xs:float array -> incx:int ->
   *             ys:float array -> incy:int ->
   *             c:float -> s:float ->
   *             unit *)

  val dswap : n:int ->
              xs:float array -> incx:int ->
              ys:float array -> incy:int ->
              unit

  val dscal : n:int -> alpha:float ->
              xs:float array -> incx:int ->
              unit

  val dcopy : n:int ->
              xs:float array -> incx:int ->
              ys:float array -> incy:int ->
              unit

  val daxpy : n:int -> alpha:float -> 
              xs:float array -> incx:int ->
              ys:float array -> incy:int ->
              unit

  val ddot : n:int ->
             xs:float array -> incx:int ->
             ys:float array -> incy:int ->
             float

  (* val dsdot : n:int ->
   *             xs:float array -> incx:int ->
   *             ys:float array -> incy:int ->
   *             float *)

  val dnrm2 : n:int ->
              xs:float array -> incx:int ->
              float
    
  (* val dznrm2 : n:int ->
   *              xs:float array -> incx:int ->
   *              float *)
    
  val dasum : n:int ->
              xs:float array -> incx:int ->
              float

  val idamax : n:int ->
               xs:float array -> incx:int ->
               int

end = struct
  
  let drotg ~alpha ~beta ~c ~s =
    let abs_alpha = abs_float !alpha in
    let abs_beta = abs_float !beta in
    let roe = if (abs_alpha > abs_beta) then !alpha else !beta in
    let scale = abs_alpha +. abs_beta in
    let r = ref 0. in
    let z = ref 0. in
    let helper () = 
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
        helper();
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
      for _i = 0 to n-1 do
        xs.(!ix) <- alpha *. xs.(!ix);
        ix := !ix + incx;
      done

  let dcopy ~n ~xs ~incx ~ys ~incy = 
    if n > 0 then
      let ix = ref (if incx < 0 then (1-n)*incx else 0) in
      let iy = ref (if incy < 0 then (1-n)*incy else 0) in
      for _i = 0 to n-1 do
        ys.(!iy) <- xs.(!ix);
        ix := !ix + incx;
        iy := !iy + incy;
      done

      
  let daxpy ~n ~alpha ~xs ~incx ~ys ~incy = 
    if n > 0 && alpha != 0. then
      let ix = ref (if incx < 0 then (1-n)*incx else 0) in
      let iy = ref (if incy < 0 then (1-n)*incy else 0) in
      for _i = 0 to n-1 do
        ys.(!iy) <- ys.(!iy) +. alpha *. xs.(!ix);
        ix := !ix + incx;
        iy := !iy + incy;
      done

  let ddot ~n ~xs ~incx ~ys ~incy =
    let dtemp = ref 0. in 
    let helper () =
      if n > 0 then (
        let ix = ref (if incx < 0 then (1-n)*incx else 0) in
        let iy = ref (if incy < 0 then (1-n)*incy else 0) in
        for _i = 0 to n-1 do
          dtemp := !dtemp +. ys.(!iy) *. xs.(!ix);
          ix := !ix + incx;
          iy := !iy + incy;
        done
      ) in (
        helper();
        !dtemp
      )
         
  let dnrm2 ~n ~xs ~incx =
    let dp = ddot ~n:n ~xs:xs ~incx:incx ~ys:xs ~incy:incx in
    sqrt dp
    
  let dasum ~n ~xs ~incx = 
    let dtemp = ref 0. in 
    let helper () =
      if n > 0 && incx > 0 then (
        let ix = ref 0 in
        for _i = 0 to n-1 do
          dtemp := !dtemp +. abs_float xs.(!ix);
          ix := !ix + incx;
        done
      ) in (
        helper();
        !dtemp
      )

  let idamax ~n ~xs ~incx = 
    let idmax = ref 0 in  
    let helper () =
      if n < 1 || incx <= 0 then (
        idmax := 0;
      ) else if n == 1 then (
        idmax := 1;
      ) else (
        let ix = ref 0 in      
        let dmax = ref (abs_float xs.(!ix)) in
        ix := !ix + incx;
        for i = 1 to n-1 do
          if (abs_float xs.(!ix) > !dmax) then
            idmax := i;
          dmax := abs_float xs.(!ix);
          ix := !ix + incx;
        done
      ) in (
        helper();
        !idmax
      )
             
end
