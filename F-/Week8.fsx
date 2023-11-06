type Book = string
type Shelf = Book list // ordered alphabetically
type Date = int
type Name = string
type Loan = Book * Name * Date

let sh0 = ["Introduction to meta-mathematics";
    "To mock a mockingbird";
    "What is the name of this book"];;

let ls0 = [("Communication and concurrency", "Bob", 4);
    ("Programming in Haskell", "Paul", 2);
    ("Communicating Sequential processes", "Mary", 7);
    ("Elements of the theory of computation", "Dick", 1)];;

// Question 1
let rec onShelf (book:string) (shelf: string list) =
    match book,shelf with
    | _,[] -> false
    | (x: string),y::t when x=y -> true
    | (x: string), y::t -> onShelf x t;;

let rec toShelf (book:string) (shelf: string list) =
    match book,shelf with
    | (x: string),[] -> x::[]
    | (x: string),y::t when x<y -> x::y::t 
    | (x: string), y::t -> y::toShelf x t;;

let rec fromShelf (book:string) (shelf: string list) =
    match book,shelf with
    | (x: string),[] -> None
    | (x: string),y::t when x=y -> Some(t) 
    | (x: string), y::t -> match fromShelf x t with
                           | None -> None
                           | Some t' -> Some (y::t');;

let rec addLoan (b:string) (n:string) (d:int) (ls:(string*string*int) list)  =
    match b,n,d,ls with
    | (a: string),(b: string),(c: int),(xs: (string * string * int) list) -> (a,b,c)::xs;;

let rec removeLoan (b: string) (n: string) (ls:(string*string*int) list) =
    match b,n,ls with
    | _,_,[] -> failwith "Loan not found"
    | (b: string),(n: string),(x,y,z)::xs when (b,n) = (x,y)-> xs
    | (b: string),(n: string),(x,y,z)::xs -> (x,y,z)::removeLoan b n xs;;

let rec reminders (d0:int) (ls: (string * string * int) list) =
    match d0,ls with
    | (date:int),(b,n,d)::xs when d<date -> (n,b)::reminders d xs
    | _,_::xs -> reminders d0 xs
    | _,[] -> [];;

let rec letters (ls: (string * string) list) =
    match ls with
    | (n,b)::xs -> ("Dear "+n+"!\nPlease return \""+b+"\".\nRegards Robin")::letters xs
    | [] -> [];;
    