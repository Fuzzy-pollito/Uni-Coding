let rec fact (n: int) =
    if n=0 then 1
    else n * fact(n-1);;

printfn "%A" (fact(4))
