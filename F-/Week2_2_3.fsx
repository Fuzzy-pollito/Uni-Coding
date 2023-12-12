let isIthChar: string * int * char -> bool = function
    |(s: string,i: int,cha: char) when s[i]=cha -> true
    |_ -> false
    ;;