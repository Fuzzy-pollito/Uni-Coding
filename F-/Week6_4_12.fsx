
(*
let  p (x: int) =
    if x>0 then true
    else false;;
    *)
let rec sump (p: int -> bool) (xs: int list) =
    match xs with
    | [] -> 0
    | x::t when p x -> x + sump p t
    | x::t -> sump p t;;

    //[1;2;-4;5;-3]

List.fold
let sum (p) (xs: int list) =
    List.fold (fun result x -> if p x then x+result else result) 0 xs;;

