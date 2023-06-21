namespace Practice 

module Library = 
    open System
    let rec fibTail a b n = 
        match n with
            | 1 -> 1
            | _ -> b + fibTail b (a + b) (n - 1)
      
    let isEven (x : Int64) =
            x % 2L = 0L 

    let getPrimeFactors (number : int64) =
        let rec inner (number : int64) (possible : int64) factors =
            if possible = number then
                possible :: factors
            elif number % possible = 0L then
                inner (number / possible) possible (possible :: factors)
            else
                inner number (possible + 1L) factors

        inner number 2L []
          
    let isPalindrome x =
        let n = x.ToString()
        let num = Seq.toList n 
        let rev = num |> List.rev
        num = rev

    let getPower (n : Int64) (b : Int64) =
        let b' = b |> float
        let n' = n |> float
        let exp = Math.Log (n',b') |> int
        pown b exp
        
    let cartesianProduct a b =
        a |> List.collect (fun x -> b |> List.map (fun y -> (x, y)))
        