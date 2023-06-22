namespace Practice 

module Library = 
    open System
    open System.IO
    
    let rec fibTail a b n = 
        match n with
            | 1 -> 1L
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
        
    (* works on smaller matrices but doesn't scale - sorry :( ran out of time *)
    let shortestPath (matrix : int List List) =
        let start = matrix.[0].[0]
        let rowSize = matrix.Length - 1
        let columnSize = matrix.[0].Length - 1
        
        let rec inner2 row column value =
            let v = matrix.[row].[column]
            match (row, column) with
                | _ when (row = rowSize) && (column = columnSize) ->            
                    value
                | _ when (row = rowSize) -> inner2 row (column + 1) (value + matrix[row][column + 1])
                | _ when (column = columnSize) -> inner2 (row + 1) column (value + matrix[row+1][column])  
                | _ -> 
                    let path1 = inner2 row (column + 1) (value + matrix.[row].[column + 1])
                    let path2 = inner2 (row + 1) column (value + matrix.[row + 1].[column])
                    
                    if path1 > path2 then inner2 (row + 1) column (value + matrix.[row + 1].[column])
                    else inner2 row (column + 1) (value + matrix.[row].[column + 1])     
        inner2 0 0 start
        
    
    let rowFromString (row : string)  =
        let rowSeq = row.Split ','
        rowSeq
            |> Seq.map (fun i -> i.Trim())
            |> Seq.map int
            |> Seq.toList
     
    let getMatrix fp =
        let matrix =
            File.ReadLines fp
            |> Seq.map rowFromString
            |> Seq.toList     
        shortestPath matrix 