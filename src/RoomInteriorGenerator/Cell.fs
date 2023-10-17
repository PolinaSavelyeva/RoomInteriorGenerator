module RoomInteriorGenerator.Cell

type CellStatus =
    | Occupied
    | Tentative
    | NonOccupied

type Cell =
    val Status: CellStatus
    val XCoordinate: int
    val YCoordinate: int

    new (status, row, column) =
        {
            Status = status
            XCoordinate = row
            YCoordinate = column
        }
