
val dswap : n:int ->
            xs:float array -> incx:int ->
            ys:float array -> incy:int ->
            unit

val dcopy : n:int ->
            xs:float array -> incx:int ->
            ys:float array -> incy:int ->
            unit

val daxpy : n:int -> alpha:float -> 
            xs:float array -> incx:int ->
            ys:float array -> incy:int ->
            unit

val dscal : n:int -> alpha:float ->
            xs:float array -> incx:int ->
            unit

val ddot : n:int ->
           xs:float array -> incx:int ->
           ys:float array -> incy:int ->
           float

val drotg : alpha:float ref -> beta:float ref ->
            c:float ref -> s:float ref ->
            unit

val drot : n:int ->
           xs:float array -> incx:int ->
           ys:float array -> incy:int ->
           c:float -> s:float ->
           unit

val dasum : n:int ->
            xs:float array -> incx:int ->
            float

val idamax : n:int ->
             xs:float array -> incx:int ->
             int

val dnrm2 : n:int ->
            xs:float array -> incx:int ->
            float
               
                                                     
