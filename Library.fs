namespace Practice 

module Library = 
    open System
    //testing gitignore file 
    let rec fib n =
                match n with
                | 1 -> 1
                | 2 -> 2
                | _ -> fib (n - 1) +  fib (n - 2)   
    
    let getFibs limit =
        let rec inner term acc =
            let n = fib term
            if n > limit then acc
            else
                inner (term + 1) (n::acc)
        inner 1 []
        
    let isEven x =
            x % 2 = 0 

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
        