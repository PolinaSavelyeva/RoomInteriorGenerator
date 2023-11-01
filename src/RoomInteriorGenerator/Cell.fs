module RoomInteriorGenerator.Cell

type CellStatus =
    | NonOccupied
    | AgainstTheWall
    | Occupied
    | OccupiedForChildren

type Cell =
    val mutable Status: CellStatus
    val RowIndex: int
    val ColumnIndex: int

    new(cellStatus, rowIndex, columnIndex) =
        { Status = cellStatus
          RowIndex = rowIndex
          ColumnIndex = columnIndex }

    member this.MakeOccupied = this.Status <- Occupied
    member this.MakeOccupiedForChildren = this.Status <- OccupiedForChildren
    member this.IsOccupied = this.Status = Occupied
    member this.IsAgainstTheWall = this.Status = AgainstTheWall
    member this.IsNonOccupied = this.Status = NonOccupied
    member this.IsOccupiedForChildren = this.Status = OccupiedForChildren

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

            if i >= this.Length && j >= this.Width || i < 0 || j < 0 then
                failwith "Index out of the range"
            else
                this.Data[i * this.Width + j]

    member this.ClearOccupiedForChildrenCells =
        //TODO
        Array.iter
            (fun (cell: Cell) ->
                if cell.IsOccupiedForChildren then
                    cell.MakeOccupied)
            this.Data
