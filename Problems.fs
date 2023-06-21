﻿namespace Practice
open System
open Library 
module Methods =
    
    let sumMultiples limit=
            [ 0..limit ]
            |> List.filter (fun i -> i % 3 = 0 || i % 5 = 0)
            |> List.sum
            
    (* accidentally eradicated need for Fib function? struggled to make it tail recursive with an upper limit*)
    let getFibs limit =
        let rec inner a b term acc =
            if b > limit then
                acc
            else
                inner b (a + b) (term + 1) (b :: acc)
        inner 0L 1L 1 []
        
    let sumEvenFibs limit =
        getFibs limit
            |> List.filter Library.isEven
            |> List.sum 
        
    let largestPrime (x : int64) =
        let primes = Library.getPrimeFactors x
        primes |> List.max
        
    let commonMultipleUpTo (n : Int64) =
        let upper = int n
        [2..upper]
            |> List.map int64
            |> List.map getPrimeFactors
            |> List.collect id 
            |> List.distinct
            |> List.map (fun i -> Library.getPower n i) // when I try to write as List.map Library.getPower n I get an error?    
            |> List.reduce (*)
            
     
    let MaxPalindrome digits =
        let b = 10.0 ** (digits - 1.0)
        let u = (10.0 ** digits) - 1.0
        let nums = [b..u]
        nums
        |> cartesianProduct nums   
        |> List.map (fun (x,y) -> x * y)
        |> List.filter isPalindrome
        |> List.max
        
module Problems = 
           
    let p1 argv =
        let limit =
            if argv = "d" then 1000
            else argv |> int 
        let answer = Methods.sumMultiples limit
        printfn $"{answer}"
    
    let p2 argv =
        let limit =
            if argv = "d" then 4000000
            else argv |> int 
        let answer = Methods.sumEvenFibs limit
        printfn $"{answer}" 

    let p3 (argv: string) = 
        let i = 
            if argv = "d" then "600851475143"
            else argv
        match Int64.TryParse(i) with
            |true, v ->
                let answer =Methods.largestPrime v
                printfn $"{answer}"
            | _ -> printfn $"Invalid String{i}" 

    let p4 (argv : string) = 
        let digits = argv |> float
        let answer = Methods.MaxPalindrome digits
        printfn $"{answer}"

    let p5 (argv : string) =
        let i = 
            if argv = "d" then "20"
            else argv
            
        match Int64.TryParse(i) with
            |true, v ->
                let answer = Methods.commonMultipleUpTo v
                printfn $"{answer}"
            | _ -> printfn $"Invalid String{i}" 

(*
Can have one solution with many projects
Make one solution
Two projects
- Library
- Problems
- read our style guide (Terence will send link).
- Set up Rider properly
- Set up F# version correctly
- Set up .NET version correctly
- Set up solution properly
- Get correct Rider version


- Factor out the sum function so that its a function on its own, call it from main
- Line 6 you don't need the "for"
- Would normally use a list or sequence

*)
