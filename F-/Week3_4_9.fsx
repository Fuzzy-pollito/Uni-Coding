let rec zip (xs: int list) (ys: int list) =
    match xs,ys with
        |[],[] -> []
        | [],_ | _,[] -> failwith "Input Error: List lengths not matching, Nigger."
        | x::xt, y::yt -> (x,y)::zip xt yt

printfn "%A" (zip [5;6;7;8] [1;2;3;4]);;