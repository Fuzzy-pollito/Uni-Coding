(*[1;2;4;6;7;7;7;9;9;13;13;14;15]*)
let rec count (xs:int list) (x:int) =
    match xs with
        | h::t when h=x -> 1 + count t x
        | h::t -> count t x
        | [] -> 0;;

let rec insert (xs:int list) (x:int) =
    match xs with
        | [] -> [x]
        | h::t when x<=h -> x::h::t
        | h::t -> h::insert t x;;


(*[1;2;4;5] [1;2;2;2;4]*)
let rec intersect (xs:int list) (ys:int list) =
    match xs,ys with
        | [],_|_,[]-> []
        | h::t,x::y when h=x -> x::intersect (h::t) y 
        | h::t,x::y when h>x -> intersect (h::t) y
        | h::t,x::y -> intersect (x::y) t;;

let rec union (xs:int list) (ys:int list) =
    match xs,ys with
        | [],_|_,[]-> xs@ys
        | h::t,x::y when h=x -> x::h::union t y 
        | h::t,x::y when h>x -> x::union (h::t) y
        | h::t,x::y -> h::union (x::y) t;;

let rec minus (xs:int list) (ys:int list) =
    match xs,ys with
        | [],_ -> []
        | _,[] -> xs
        | h::t,x::y when h=x -> minus t y 
        | h::t,x::y when h>x -> minus (h::t) y
        | h::t,x::y -> h::minus (x::y) t;;