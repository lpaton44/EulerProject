open Practice 
open System 

let runP p v =
    let programme = p |> int
    match programme with 
    | 1 -> Problems.p1 v 
    | 2 -> Problems.p2 v
    | 3 -> Problems.p3 v
    | 4 -> Problems.p4 v
    | 5 -> Problems.p5 v
    | 8 -> Problems.p8 v
    | _ -> printfn "Invalid number." 


[<EntryPoint>]
let main argv = 
        
    printf "Enter the problem you would like solved:
    1: Numbers below a limit divisible by 3 or 5 
    2: Summing Fibonacci numbers below a limit
    3: Finding largest prime factor of a number 
    4: Finding the largest number that is a palindrome and a product of two n-digit numbers 
    5: Finding the smallest common multiple of numbers 1...
    8: Finding the smallest common multiple of numbers 1...n \n" 

    let p = Console.ReadLine().Trim()
    let programme = p |> int
    let s =     
        match programme with 
            | 1 -> "Enter upper limit or d for (default = 1000): "
            | 2 -> "Enter upper limit or d for (default = 4 000 000): "
            | 3 -> "Enter value or d for (default): "
            | 4 -> "Enter the number of digits: "
            | 5 -> "Enter value or d for (default = 20): "
            | 8 -> "Enter matrix file name:  "
            | _ -> "Invalid Number."
    
    printf $"{s}" 
    let v = 
        if not (s = "") then Console.ReadLine().Trim()
        else "d"
    runP programme v
    0