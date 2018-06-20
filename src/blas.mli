
val dswap : n:int -> xs:float array -> incx:int -> ys:float array -> incy:int -> unit

val dscal : n:int -> alpha:float -> xs:float array -> incx:int -> unit

val drotg : alpha:float ref -> beta:float ref -> c:float ref -> s:float ref -> unit

val drot : n:int ->
           xs:float array -> incx:int ->
           ys:float array -> incy:int ->
           c:float -> s:float ->
           unit
                                                                                 
                                                     
