let rec sum (n: int) =
    if n=0 then 0
    else n + sum(n-1);;

printfn "%A" (sum(4))
