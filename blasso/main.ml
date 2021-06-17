module B = Blasso.Blas

let _ =
  let x0 = [| 0.; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9. |] in
  let x1 = [| 0.; 1.; 2.; 3.; 400.; 5.; 6.; 7.; 8.; 9. |] in
  let y0 = Array.map (fun y -> 2. *. y) (Array.copy x0) in
  let s0 = 570. in
  let d = B.Dot {
      xs=x0 |> B.Zipper.of_array ~index:0;
      ys=y0 |> B.Zipper.of_array ~index:0;
      result=(Some s0);
    } in
  let s1 = match B.update d ~index:4 ~value:(Array.get x1 4) with
    | B.Dot {result=Some v; _} -> v
    | _ -> raise (Sys_error "Unknown Blas op")
  in
  let ss = Printf.sprintf "s0: %f, s1: %f, 3738." s0 s1 in
  print_endline ss
