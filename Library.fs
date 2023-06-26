namespace Practice 

module Library = 
    open System
    open System.IO
    open System.Collections.Generic
    
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
        
    let shortestPath (matrix: int list list) =
        let start = matrix.[0].[0]
        let rowSize = List.length matrix - 1
        let columnSize = List.length matrix.[0] - 1
        
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
           
    type Node = {
        coord: int*int
        mutable cost: int
        mutable parent: Node Option       
    }
    
    let dijkstra (matrix: int list list) =
        
        let rowCount = List.length matrix - 1
        let columnCount = List.length matrix.[0] - 1
         
        let nodes = Array2D.init (rowCount + 1) (columnCount + 1) (fun i j ->
            if i = 0 && j = 0 then
                { coord = (0, 0); cost = matrix.[0].[0]; parent = None }
            else
                { coord = (i, j); cost = Int32.MaxValue; parent = None }
        )
        
        let mutable visited = Array2D.create (rowCount + 1) (columnCount + 1) false

        let mutable queue = [nodes.[0,0]]
        
        let getAdjacentNodes node =
            let i, j = node.coord
            match i, j with
            | row, col when (row = rowCount && col = columnCount) -> []
            | row, _ when row = rowCount -> [(i, j + 1)]
            | _, col when col = columnCount -> [(i+1, j)]
            | _,_ ->
                [(i, j + 1); (i + 1, j)]
        
        let addToQueue node =
            queue <- node :: queue

        let getFromQueue () =
            let node = List.head queue
            queue <- List.tail queue
            node
            
        let checkAdjacent currentNode (i, j)=
            let neighborNode = nodes.[i,j]
            let newCost = currentNode.cost + matrix[i][j]
            
            if newCost < neighborNode.cost then
                neighborNode.cost <- newCost
                neighborNode.parent <- Some currentNode
                addToQueue neighborNode
                            
        while queue <> [] do           
            let currentNode = getFromQueue ()
            let x, y = currentNode.coord
            
            if not (visited.[x, y]) then
                visited.[x, y] <- true
                
                for adj in getAdjacentNodes currentNode do
                    let (i, j) = adj
                    checkAdjacent currentNode (i, j)
                    
        let dest = nodes.[rowCount, columnCount]
        dest.cost 
         
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
        dijkstra matrix 