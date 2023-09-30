let rec fib (n:int) =
    match n with
    | 0 -> 0
    | 1 -> 1
    | (n :int) -> fib(n-1) + fib(n-2)


printfn "%A" (fib(4))