module RoomInteriorGenerator.PCG

open DataTable
open Cell

let selectObjectToPlace (dataTable: DataTable<'Value>) =

    fun randomIntGeneratorWithSeed ->
        let objectToPlace = dataTable[randomIntGeneratorWithSeed 0 dataTable.Length]

        let objectToPlaceInstance =
            List.item (List.length objectToPlace.Instances |> randomIntGeneratorWithSeed 0) objectToPlace.Instances

        objectToPlace, objectToPlaceInstance

let findAvailablePlaceForObject (cellGrid: CellGrid) (chosenObject: DataTableRow<'Value> * ObjectVariant<'Value>) =

    let isFitting (instance: ObjectVariant<'Value>) (cellRowIndex, cellColumnIndex) =
        let mutable isFitting = true

        let diameterWidth = instance.ColliderWidth / 2

        let diameterLength = instance.ColliderLength / 2

        for i in cellColumnIndex - diameterLength .. cellColumnIndex + diameterLength do
            for j in cellRowIndex - diameterWidth .. cellRowIndex + diameterWidth do
                if cellGrid[i, j].IsOccupied then
                    isFitting <- false

        isFitting

    let chooseFittingCells predicate = Array.choose predicate cellGrid.Data

    let makePredicate =
        match (fst chosenObject).Rules with
        | Node(rule) ->
            match rule with
            | NodePlacementRule.AgainstTheWall -> (fun (cell: Cell) -> if cell.IsAgainstTheWall then Some cell else Option.None)
            | None ->
                (fun (cell: Cell) ->
                    if cell.IsNonOccupied || cell.IsAgainstTheWall then
                        Some cell
                    else
                        Option.None)

    let generateCellIndexes randomIntGeneratorWithSeed (fittingCellsArray: array<Cell>) =
        let cell = fittingCellsArray[randomIntGeneratorWithSeed 0 fittingCellsArray.Length]

        cell.RowIndex, cell.ColumnIndex

    fun randomIntGeneratorWithSeed ->

        let fittingCellsArray = chooseFittingCells makePredicate

        if Array.isEmpty fittingCellsArray then
            Option.None
        else
            let mutable continueLooping = true
            let mutable cellIndexes = (0, 0)

            while continueLooping do

                cellIndexes <- generateCellIndexes randomIntGeneratorWithSeed fittingCellsArray

                if isFitting (snd chosenObject) cellIndexes then
                    continueLooping <- false

            Some cellIndexes

let generateInterior (cellGrid: CellGrid) (dataTable: DataTable<'Value>) (maximumAmountOfObjects: int) placementFunction =

    let _makeOccupied (instance: ObjectVariant<'Value>) (cellRowIndex, cellColumnIndex) =

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
                _makeOccupied (snd object) place.Value

                amountOfObjectsToBePlaced <- amountOfObjectsToBePlaced - 1
