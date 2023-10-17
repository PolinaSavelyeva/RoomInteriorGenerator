module RoomInteriorGenerator.PCG
open Cell
open DataTable
open RoomInteriorGenerator.DataTable

let chooseObjectToPlace (objectToPlaceList: DataTable) =

    let _generateProbabilisticMap = Array.init 0

    0

let chooseAvailableCellsForObject (cellList: List<Cell>) (chosenObject: DataTableRow * int) =

    let _clearCellList = 0

    let _makeCellsOccupied = 0

    let _isFitting (xCoordinate: int) (yCoordinate: int) : bool =
        let checkBorders = 0
        true

    if _isFitting xCoordinate yCoordinate then (chosenObject: DataTableRow * int), (xCoordinate, yCoordinate)
    else _isFitting <| generateCellCoordinates

let generateInterior (cellList: List<Cell>) (dataTable: DataTable) (maximumAmountOfObjects: int) placeFunction =

    while maximumAmountOfObjects <> 0 do
        let object = chooseAvailableCellsForObject cellList
        let place = object |> chooseAvailableCellsForObject cellList
        placeFunction object place
        maximumAmountOfObjects -= 1

        while !object.ChildList.isEmpty && maximumAmountOfObjects <> 0 do
                let object = chooseAvailableCellsForObject cellList
                let place = object |> chooseAvailableCellsForObject cellList
                placeFunction object place
                maximumAmountOfObjects -= 1
                




