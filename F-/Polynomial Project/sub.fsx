let rec mult (m: int, cs: int list) = 
    match m,cs with
    | _,[] -> []
    | (m: int),h::t -> (m*h)::mult(m,t)

let rec sub (c1: int list, c2: int list) = 
    match c1,c2 with
    | [],[] -> []
    | [],(c2: int list) -> mult(-1,c2)
    | (c1: int list),[] -> c1
    | x::y,h::t -> (x-h)::sub(y,t)
    

printfn "%A" (sub([9;2;3;4],[5;6;7;6;8;0]));;