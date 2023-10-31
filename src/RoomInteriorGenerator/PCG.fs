module RoomInteriorGenerator.PCG

open DataTable
open Cell
open Helper

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

    let selectPotentiallyMatchingCells predicate =
        LimitedLengthArray(Array.choose predicate cellGrid.Data)

    let isCellMatching (instance: ObjectVariant<'Value>) (cell: Cell) (fittingCellsArray: LimitedLengthArray<Cell>) indexInFittingCellsArray =
        let mutable isFitting = true

        let cellColumnIndex = cell.ColumnIndex
        let cellRowIndex = cell.RowIndex

        for i in cellColumnIndex - instance.freeCellsOnTheLeft .. cellColumnIndex + instance.freeCellsOnTheRight do
            for j in cellRowIndex - instance.freeCellsOnTheTop .. cellRowIndex + instance.freeCellsOnTheBottom do
                if cellGrid[i, j].IsOccupied then
                    fittingCellsArray.Delete indexInFittingCellsArray
                    isFitting <- false

        isFitting

    let selectCell randomIntGeneratorWithSeed (fittingCellsArray: LimitedLengthArray<Cell>) =
        // index in fittingCellsArray is not the same as cell.ColumnIndex or cell.RowIndex
        let indexInFittingCellsArray = randomIntGeneratorWithSeed 0 fittingCellsArray.Length
        fittingCellsArray[indexInFittingCellsArray], indexInFittingCellsArray

    fun randomIntGeneratorWithSeed ->

        let fittingCellsArray = selectPotentiallyMatchingCells makePredicate

        if fittingCellsArray.IsEmpty then
            Option.None
        else
            let rec inner () =
                let cell, index = selectCell randomIntGeneratorWithSeed fittingCellsArray

                if isCellMatching selectedObjectInstance cell fittingCellsArray index then
                    Some(cell.RowIndex, cell.ColumnIndex)
                else
                    inner ()

            inner ()

let generateInterior (cellGrid: CellGrid) (dataTable: DataTable<'Value>) (maximumAmountOfObjects: int) placementFunction =

    let makeOccupied (instance: ObjectVariant<'Value>) (cellRowIndex, cellColumnIndex) =

        for i in cellColumnIndex - instance.freeCellsOnTheLeft .. cellColumnIndex + instance.freeCellsOnTheRight do
            for j in cellRowIndex - instance.freeCellsOnTheTop .. cellRowIndex + instance.freeCellsOnTheBottom do
                cellGrid[i, j].MakeOccupied

    fun randomIntGeneratorWithSeed ->

        let rec inner amountOfObjectsToBePlaced =
            if amountOfObjectsToBePlaced = 0 then
                ()
            else
                let object = selectObjectToPlace dataTable randomIntGeneratorWithSeed
                let place = findAvailablePlaceForObject cellGrid object randomIntGeneratorWithSeed

                if place.IsSome then
                    placementFunction object place.Value
                    makeOccupied (snd object) place.Value

                    inner (amountOfObjectsToBePlaced - 1)
                else
                    inner amountOfObjectsToBePlaced

        inner maximumAmountOfObjects
