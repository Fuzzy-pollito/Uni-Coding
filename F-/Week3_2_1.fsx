let div(n: int)=
    let a: bool = false
    match n with
    | (n: int) when (n%5)=0 -> false
    | (n: int) when (n%2)=0 -> true 
    | (n: int) when (n%3)=0 -> true
    | _ -> false
    

printfn "%A" (div(30));;