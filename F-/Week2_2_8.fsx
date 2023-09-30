let rec pas (n: int, k: int) = 
    match (n,k) with
    | ((n: int),0) -> 1
    | ((n: int),(k: int)) when k=n -> 1
    | ((n: int),(k: int)) when k>n -> failwith "Input Error: k cannot be greater than n"
    | ((n: int),(k: int)) -> pas(n-1,k-1) + pas(n-1,k)

printfn "%A" (pas(2,4));;