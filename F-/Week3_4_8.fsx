let rec split (xs: int list) =
    match xs with
    |[] -> ([],[])
    |[x: int] ->([x],[])
    |x::y::t-> 
        let (evens: int list, odds: int list) = split t
        x::evens , y::odds

printfn "%A" (split([5;6;7;8;9]));;