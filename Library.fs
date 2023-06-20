namespace Practice 

module Library = 
    open System
    
    let sumMultiples limit=
            [ 0..limit ]
            |> List.filter (fun i -> i % 3 = 0 || i % 5 = 0)
            |> List.sum

    let rec fib n =
                match n with
                | 1 -> 1
                | 2 -> 2
                | _ -> fib(n-1) +  fib(n-2)   
    
    let isEven x =
            x % 2 = 0 

    let sumEvenFibs limit =
        let rec inner term acc =
            let n = fib term
            if n > limit then acc
            else
                let newAcc =
                    if isEven n then n::acc
                    else acc
                inner (term + 1) newAcc
        inner 1 [] |> List.sum 
    
    (* was going to make this an inner method in largestPrime but it is needed somewhere else*)
    let rec getPrimeFactors (number: int64) possible factors =
        if possible = number then
            possible::factors
        elif number % possible = 0L then
            getPrimeFactors (number/possible) possible (possible::factors)
        else
            getPrimeFactors number (possible + 1L) factors
 
    let largestPrime (x :int64) =
        getPrimeFactors x 2L [] |> List.max

    let isPalindrome x =
        let n = x.ToString()
        let num = Seq.toList n 
        let rev = num |> List.rev
        num = rev

    let rec getAllFactors (n :Int64) factors =
        if n = 2L then
            factors
        else
            let fs = getPrimeFactors (n-1L) 2L factors
            getAllFactors (n-1L) fs

    let getPower n b =
        let b' = b |> float
        let n' = n |> float
        let exp = Math.Log(n',b') |> int
        pown b exp

    let commonMultiple (n :Int64) =
        getAllFactors (n+1L) []
        |> List.distinct
        |> List.map (getPower n)
        |> List.reduce (*)
        
    let cartesianProduct a b =
        a |> List.collect (fun x -> b |> List.map (fun y -> (x,y)))
        
    let MaxPalindrome =
        let nums = [100..999]
        nums
        |> cartesianProduct nums   
        |> List.map (fun (x,y) -> x * y)
        |> List.filter isPalindrome
        |> List.max