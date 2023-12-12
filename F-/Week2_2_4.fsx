let rec occFromIth: string * int * char -> int = function
|(a: string,b: int,c: char) when b>=a.Length -> 0
|(a: string,b: int,c: char) when a[b]=c -> 1 +  occFromIth(a,b+1,c)
|(a: string,b: int,c: char) -> 0 + occFromIth(a,b+1,c)
;