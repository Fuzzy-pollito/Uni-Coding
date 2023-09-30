type lid = string
type flight = string
type airport = string
type route = (flight * airport) list
type luggageCatalogue = (lid * route) list
let rec findroute (lid: string) (luggageCatalogue: (string * ((string * string) list)) list) =
    match luggageCatalogue with
    | [] -> failwith "No matches"
    | (x,y)::t when lid = x -> y
    | x::t -> (findroute lid t);;

let rec inRoute (flight:string) (route:(string*string) list) =
    match route with
    | (x,y)::_ when flight=x -> true
    | x::t -> inRoute flight t
    | [] -> false;;

let rec withFlight  (f:string) (lc:(string * ((string * string) list)) list) =
    match lc with
    | [] -> []
    | (x,y)::t when (inRoute f y) = true -> x::withFlight f t
    | x::t -> (withFlight f t);;

//Lecture - notes

//type shape

type Shape =
    |Circle of float
    |Square of float
    |Triangle of float*float*float;;

Circle(1.0);;
Square(1.0);;
Triangle(4,3,2);;

let area (shape: Shape) = function
    | Circle(r:float) -> System.Math.PI*r**2
    | Square(l:float) ->4*l
    | Triangle(b:float,h:float,w:float) ->
        let (s:float) = (b+h+w)/2
        System.Math.Sqrt(s*w+s*h+s*b);;




let lc1: (string * (string * string) list) list =[("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]);("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")])];;