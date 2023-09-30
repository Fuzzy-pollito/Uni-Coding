let rec help f (b: int) (xs: int list)=
    match xs with
    | [] -> b
    | h::t -> f h (help f b t) 

let addE (xs: int list) = help (+) 0 xs

printfn "%A" (addE([5;6;7;6;8;0]));;
