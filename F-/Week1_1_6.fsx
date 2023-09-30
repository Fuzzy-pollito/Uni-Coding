let rec sum (m:int,n: int) =
    if n=0 then m
    else m + n + sum(m,n-1);;

printfn "%A" (sum(4,5))


