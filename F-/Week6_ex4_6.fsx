type ArticleCode = string;;
type ArticleName = string;;
type Price = int;;
type Register = (ArticleCode * (ArticleName*Price)) list;;

let (reg: (string * (string * int)) list) = [("a1",("cheese",25));
    ("a2",("herring",4));
    ("a3",("soft drink",5)) ];;

type NoPieces = int;; // np where np >= 0
type Item = NoPieces * ArticleCode;;
type Purchase = Item list;;
type Info = NoPieces * ArticleName * Price;;
type Infoseq = Info list;;
type Bill = Infoseq * Price;;

(*  let (reg: (string * (string * int)) list) = [("a1",("cheese",25))
    ("a2",("herring",4));
    ("a3",("soft drink",5)) ];;  *)

let (pur: (int * string) list) = [(3,"a2"); (1,"a1")];;

let rec bill (pur: (int * string) list) (reg: (string * (string * int)) list) =
    match pur,reg with
    | _,[] -> []
    | [],_ -> []
    | x::t,y::s -> List.fold (fun result x -> if snd(x) = fst(y) then result@[fst(snd(y))] else result@[]) [] pur;;

 bill pur reg;; 