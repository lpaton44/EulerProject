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
    
    
    [<Struct>]
    type StructPoint = 
        {
          X: int
          Y: int
        }
               
    type Node = {
        mutable cost: int
        mutable parent: StructPoint Option       
    }
    
    type State = {
        currentNode: StructPoint
        visited: StructPoint Set
        distances: Map <StructPoint, Node>  
    }
    
    let minOfMap (map : Map<_,_>) (set : StructPoint Set) =
        map |> Map.filter (fun k v -> set.Contains k ) |> Seq.minBy (fun pair-> pair.Value) 

    let getAdjacentNodes coords rowCount columnCount =
            let i,j = coords.X, coords.Y
            match i, j with
            | row, col when (row = rowCount && col = columnCount) -> []
            | row, _ when row = rowCount -> [{X = i; Y = j + 1}]
            | _, col when col = columnCount -> [{X = i + 1; Y =  j}]
            | _,_ ->
                [{X = i; Y = j + 1}; {X = i + 1; Y = j}]
    
    
    let dijkstra (matrix: int list list) =
        
        let rowCount = List.length matrix - 1
        let columnCount = List.length matrix.[0] - 1
        
        let mutable values = Map.empty
        let mutable unvisited = Set.empty
        let mutable distances = Map.empty
        
        let start = {X = 0; Y = 0}
        let startVal = matrix.[0][0] 
        let startNode = {cost = startVal; parent = None}
        distances <- distances |> Map.add start startNode
         
        for i in 0..rowCount do
            for j in 0..columnCount do
                let key = {X = i; Y = j}
                let value = matrix.[i].[j]
                unvisited <- unvisited |> Set.add key
                values <- values |> Map.add key value
        
        let rec inner currentNode =
          let neighbours = getAdjacentNodes currentNode rowCount columnCount
          let currentNodeCost =
                match distances.TryFind currentNode with
                | Some node -> node.cost
                | None -> -1
                    
          if unvisited.Count = 1 || not (unvisited.Contains currentNode) then
              None 
          else          
              for neighbour in neighbours do
                if unvisited.Contains neighbour then
                    let neighbourNodeDistance = // find distance to node if it exists 
                      match distances.TryFind neighbour with
                      | Some node -> node.cost
                      | None -> -1
                    let neighbourNodeCost = values.[neighbour]
                    let newCost = currentNodeCost + neighbourNodeCost// value of path using currrentNode as parent 
                    let newNeighbourNode = {cost = newCost; parent = Some currentNode} // new node using current node's path
                    
                    if (neighbourNodeDistance = -1) then //if no value exists add one
                        distances <- distances |> Map.add neighbour newNeighbourNode
                    elif newCost < neighbourNodeDistance then //if new value is shorter path, replace old one
                        distances <- distances |> Map.change neighbour (fun _ -> Some newNeighbourNode)
                   
              unvisited <- unvisited |> Set.remove currentNode
              let nextNode = minOfMap distances unvisited
              inner nextNode.Key
   
        let m = inner start
        let final = distances.[{X = rowCount; Y = columnCount}]
        final.cost
            
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