module RoomInteriorGenerator.Cell

/// <summary>
/// Enumerates the possible states of a cell and represents it.
/// </summary>
type Cell =
    | NonOccupied
    | AgainstTheWall // Cell is a part of the perimeter and serves as a wall
    | Occupied
    | OccupiedForChildren // Cell is occupied and reserved for child objects

/// <summary>
/// Represents a grid of cells with specified dimensions.
/// </summary>
type CellGrid =
    val Data: array<Cell>
    val Length: int
    val Width: int

    new(data, length, width) =
        { Data = data
          Length = length
          Width = width }

    /// <summary>
    /// Gets the cell from the cell grid at the specified coordinates (i, j).
    /// </summary>
    member this.Item
        with get (i, j) =
            if i >= this.Width && j >= this.Length || i < 0 || j < 0 then
                failwith "Index out of the range"
            else
                this.Data[i * this.Length + j]

    /// <summary>
    /// Cleans cells marked as OccupiedForChildren, resetting them to the Occupied state.
    /// </summary>
    member this.CleanOccupiedForChildrenCells =

        Array.iteri
            (fun n (cell: Cell) ->
                match cell with
                | OccupiedForChildren -> this.Data[n] <- Occupied)
            this.Data

    /// <summary>
    /// Marks the cell at the specified coordinates (i, j) as Occupied.
    /// </summary>
    member this.MakeOccupied(i, j) =
        this.Data[i * this.Length + j] <- Occupied

    /// <summary>
    /// Marks the cell at the specified coordinates (i, j) as OccupiedForChildren.
    /// </summary>
    member this.MakeOccupiedForChildren(i, j) =
        this.Data[i * this.Length + j] <- OccupiedForChildren

    /// <summary>
    /// Checks if the cell at the specified coordinates (i, j) is in the Occupied state.
    /// </summary>
    member this.IsOccupied(i, j) =
        this.Data[i * this.Length + j] = Occupied

    /// <summary>
    /// Checks if the cell at the specified coordinates (i, j) is in the AgainstTheWall state.
    /// </summary>
    member this.IsAgainstTheWall(i, j) =
        this.Data[i * this.Length + j] = AgainstTheWall

    /// <summary>
    /// Checks if the cell at the specified coordinates (i, j) is in the NonOccupied state.
    /// </summary>
    member this.IsNonOccupied(i, j) =
        this.Data[i * this.Length + j] = NonOccupied

    /// <summary>
    /// Checks if the cell at the specified coordinates (i, j) is in the OccupiedForChildren state.
    /// </summary>
    member this.IsOccupiedForChildren(i, j) =
        this.Data[i * this.Length + j] = OccupiedForChildren
