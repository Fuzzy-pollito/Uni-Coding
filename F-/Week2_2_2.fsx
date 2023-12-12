let rec pow: string * int -> string = 
    function
    | (a: string,b: int) when b>0 -> a + pow (a,b-1)
    | _,_ -> ""
    ;;
