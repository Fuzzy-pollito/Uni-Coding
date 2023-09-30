let rec mult (m: int, cs: int list) = 
    match m,cs with
    | _,[] -> []
    | (m: int),h::t -> (m*h)::mult(m,t)
    

printfn "%A" (mult(0,[5;6;7;6;8;0]));;
