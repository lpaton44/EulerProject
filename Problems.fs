namespace Practice
open System
open Library 

module Problems = 
    
    let p1 argv =
        let limit =
            if argv = "d" then 1000
            else argv |> int 
        let answer = Library.sumMultiples limit
        printfn $"{answer}"
    
    let p2 argv =
        let limit =
            if argv = "d" then 4000000
            else argv |> int 
        let answer = Library.sumEvenFibs limit
        printfn $"{answer}" 

    let p3 (argv: string) = 
        let i = 
            if argv = "d" then "600851475143"
            else argv
        match Int64.TryParse(i) with
            |true, v ->
                let answer =Library.largestPrime v
                printfn $"{answer}"
            | _ -> printfn $"Invalid String{i}" 

    let p4 argv = 
        let answer = Library.MaxPalindrome 
        printfn $"{answer}"

    let p5 (argv : string) =
        let i = 
            if argv = "d" then "20"
            else argv
            
        match Int64.TryParse(i) with
            |true, v ->
                let answer =Library.commonMultiple v
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
