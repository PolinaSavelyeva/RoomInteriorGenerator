module RoomInteriorGenerator.Cell

type Cell =
    | NonOccupied
    | AgainstTheWall
    | Occupied
    | OccupiedForChildren

type CellGrid =
    val Data: array<Cell>
    val Length: int
    val Width: int

    new(data, length, width) =
        { Data = data
          Length = length
          Width = width }

    member this.Item (i, j) =
        if i >= this.Width && j >= this.Length || i < 0 || j < 0 then
            failwith "Index out of the range"
        else
            this.Data[i * this.Length + j]
    member this.ClearOccupiedForChildrenCells =
        //TODO
        Array.iteri
            (fun n (cell: Cell) ->
                match cell with
                | OccupiedForChildren -> this.Data[n] <- Occupied
                | _ -> ())
            this.Data

    member this.MakeOccupied (i, j) = this[i,j] <- Occupied
    member this.MakeOccupiedForChildren (i, j) = this[i,j] <- OccupiedForChildren
    member this.IsOccupied (i, j) = this[i,j] = Occupied
    member this.IsAgainstTheWall (i, j) = this[i,j] = AgainstTheWall
    member this.IsNonOccupied (i, j) = this[i,j] = NonOccupied
    member this.IsOccupiedForChildren (i, j) = this[i,j] = OccupiedForChildren
