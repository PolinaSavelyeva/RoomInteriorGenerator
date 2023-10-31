module RoomInteriorGenerator.PCG

open DataTable
open Cell

let selectObjectToPlace (dataTable: DataTable<'Value>) =

    fun randomIntGeneratorWithSeed ->
        let objectToPlace = dataTable[randomIntGeneratorWithSeed 0 dataTable.Length]

        let objectToPlaceInstance =
            objectToPlace.Instances[(randomIntGeneratorWithSeed 0 objectToPlace.LengthOfInstancesArray)]

        objectToPlace, objectToPlaceInstance

let findAvailablePlaceForObject (cellGrid: CellGrid) (selectedObjectRow: DataTableRow<'Value>, selectedObjectInstance: ObjectVariant<'Value>) =

    let makePredicate =
        match selectedObjectRow.Rules with
        | Node(rule) ->
            match rule with
            | NodePlacementRule.AgainstTheWall -> (fun (cell: Cell) -> if cell.IsAgainstTheWall then Some cell else Option.None)
            | None ->
                (fun (cell: Cell) ->
                    if cell.IsNonOccupied || cell.IsAgainstTheWall then
                        Some cell
                    else
                        Option.None)

    let selectPotentiallyMatchingCells predicate = Array.choose predicate cellGrid.Data

    let isCellMatching (instance: ObjectVariant<'Value>) (cell: Cell) (fittingCellsArray: array<Cell>) fittingCellsArrayIndex =
        let mutable isFitting = true

        let diameterWidth = instance.ColliderWidth / 2
        let diameterLength = instance.ColliderLength / 2

        let cellColumnIndex = cell.ColumnIndex
        let cellRowIndex = cell.RowIndex

        for i in cellColumnIndex - diameterLength .. cellColumnIndex + diameterLength do
            for j in cellRowIndex - diameterWidth .. cellRowIndex + diameterWidth do
                if cellGrid[i, j].IsOccupied then
                    //TODO fittingCellsArrayIndex false
                    isFitting <- false

        isFitting

    let selectCell randomIntGeneratorWithSeed (fittingCellsArray: array<Cell>) =
        let index = randomIntGeneratorWithSeed 0 fittingCellsArray.Length
        fittingCellsArray[index], index

    fun randomIntGeneratorWithSeed ->

        let rec inner fittingCellsArray =
            let cell, index = selectCell randomIntGeneratorWithSeed fittingCellsArray

            if isCellMatching selectedObjectInstance cell then
                Some(cell.RowIndex, cell.ColumnIndex)
            else
                inner fittingCellsArray

        let fittingCellsArray = selectPotentiallyMatchingCells makePredicate

        if Array.isEmpty fittingCellsArray then
            Option.None
        else
            inner fittingCellsArray

let generateInterior (cellGrid: CellGrid) (dataTable: DataTable<'Value>) (maximumAmountOfObjects: int) placementFunction =

    let makeOccupied (instance: ObjectVariant<'Value>) (cellRowIndex, cellColumnIndex) =

        let diameterWidth = instance.ColliderWidth / 2

        let diameterLength = instance.ColliderLength / 2

        for i in cellColumnIndex - diameterLength .. cellColumnIndex + diameterLength do
            for j in cellRowIndex - diameterWidth .. cellRowIndex + diameterWidth do
                cellGrid[i, j].MakeOccupied

    fun randomIntGeneratorWithSeed ->

        let mutable amountOfObjectsToBePlaced = maximumAmountOfObjects

        while amountOfObjectsToBePlaced <> 0 do

            let object = selectObjectToPlace dataTable randomIntGeneratorWithSeed
            let place = findAvailablePlaceForObject cellGrid object randomIntGeneratorWithSeed

            if place.IsSome then
                placementFunction object place.Value
                makeOccupied (snd object) place.Value

                amountOfObjectsToBePlaced <- amountOfObjectsToBePlaced - 1
