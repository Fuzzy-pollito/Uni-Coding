let rec evenN (n: int) =
    match n with
    |0-> []
    |n when n%2=0-> evenN(n-2)@[n]
    |_ -> evenN(n-1)

printfn "%A" (evenN(15));;
