let rec mult (x: int, xs: int list) = 
    match (x,xs) with
    | (0,[]) -> 1
    | (_,[]) -> 0
    | ((x: int),h::t) when x=h -> 1 + mult (x,t)
    | ((x: int),h::t) -> mult (x,t)

printfn "%A" (mult(2,[2;3;4;2;2]));;