let rec add (c1: int list, c2: int list) = 
    match c1,c2 with
    | [],[] -> []
    | [],(c2: int list) -> c2
    | (c1: int list),[] -> c1
    | x::y,h::t -> (x+h)::add(y,t)
    

printfn "%A" (add([9;2;3;4],[5;6;7;6;8;0]));;
