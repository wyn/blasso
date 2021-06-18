module B = Blasso.Blas
module Z = Blasso.Zipper

let x0 = [| 0.; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9. |];;
let x1 = [| 0.; 1.; 2.; 3.; 400.; 5.; 6.; 7.; 8.; 9. |];;
let y0 = Array.map (fun y -> 2. *. y) (Array.copy x0);;
let s0 = 570.;;
let index = 4;;
let value = x1.(index);;
let coord = B.X;;
