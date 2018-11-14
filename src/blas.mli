(*
TODO test against netlib BLAS
TODO parameterise on floatiness
TODO parameterise on array container
TODO use stencils rather than (n, xs, incx) stuff
 *)

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

               
                                                     
