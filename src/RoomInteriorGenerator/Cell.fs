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

    member this.Item
        with get (i, j) =
            if i >= this.Width && j >= this.Length || i < 0 || j < 0 then
                failwith "Index out of the range"
            else
                this.Data[i * this.Length + j]

    member this.CleanOccupiedForChildrenCells =

        Array.iteri
            (fun n (cell: Cell) ->
                match cell with
                | OccupiedForChildren -> this.Data[n] <- Occupied
                | _ -> ())
            this.Data

    member this.MakeOccupied(i, j) =
        this.Data[i * this.Length + j] <- Occupied

    member this.MakeOccupiedForChildren(i, j) =
        this.Data[i * this.Length + j] <- OccupiedForChildren

    member this.IsOccupied(i, j) =
        this.Data[i * this.Length + j] = Occupied

    member this.IsAgainstTheWall(i, j) =
        this.Data[i * this.Length + j] = AgainstTheWall

    member this.IsNonOccupied(i, j) =
        this.Data[i * this.Length + j] = NonOccupied

    member this.IsOccupiedForChildren(i, j) =
        this.Data[i * this.Length + j] = OccupiedForChildren
