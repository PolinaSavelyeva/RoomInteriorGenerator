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

    let selectPotentiallyMatchingCells predicate =
        LimitedLengthArray(Array.choose predicate cellGrid.Data)

    let isCellMatching (instance: ObjectVariant<'Value>) (cell: Cell) (fittingCellsArray: LimitedLengthArray<Cell>) indexInFittingCellsArray =

        let cellColumnIndex = cell.ColumnIndex
        let cellRowIndex = cell.RowIndex

        let mutable isFitting = true
        let mutable loop = true

        while loop do
            for i in cellRowIndex - instance.freeCellsOnTheTop .. cellRowIndex + instance.freeCellsOnTheBottom do
                for j in cellColumnIndex - instance.freeCellsOnTheLeft .. cellColumnIndex + instance.freeCellsOnTheRight do
                    if i < 0 || i > cellGrid.Width || j < 0 || j > cellGrid.Length || cellGrid[i, j].IsOccupied then
                        fittingCellsArray.Delete indexInFittingCellsArray
                        loop <- false
                        isFitting <- false

            loop <- false

        isFitting

    let selectCell (fittingCellsArray: LimitedLengthArray<Cell>) =
        fun randomIntGeneratorWithSeed ->
            // index in fittingCellsArray is not the same as cell.ColumnIndex or cell.RowIndex
            let indexInFittingCellsArray = randomIntGeneratorWithSeed 0 fittingCellsArray.Length
            fittingCellsArray[indexInFittingCellsArray], indexInFittingCellsArray

    fun randomIntGeneratorWithSeed ->

        let fittingCellsArray = selectPotentiallyMatchingCells makePredicate

        let rec inner () =
            if fittingCellsArray.IsEmpty then
                Option.None
            else
                let cell, index = selectCell fittingCellsArray randomIntGeneratorWithSeed

                if isCellMatching selectedObjectInstance cell fittingCellsArray index then
                    Some(cell.RowIndex, cell.ColumnIndex)
                else
                    inner ()

        inner ()

let generateInterior (cellGrid: CellGrid) (dataTable: DataTable<'Value>) (maximumAmountOfObjects: int) placementFunction =

    let makeOccupied (instance: ObjectVariant<'Value>) (cellRowIndex, cellColumnIndex) =

        for i in cellRowIndex - instance.freeCellsOnTheTop .. cellRowIndex + instance.freeCellsOnTheBottom do
            for j in cellColumnIndex - instance.freeCellsOnTheLeft .. cellColumnIndex + instance.freeCellsOnTheRight do
                cellGrid[i, j].MakeOccupied

    fun randomIntGeneratorWithSeed ->

        let rec inner amountOfObjectsToBePlaced =
            if amountOfObjectsToBePlaced = 0 then
                ()
            else
                let objectRow, instance = selectObjectToPlace dataTable randomIntGeneratorWithSeed

                let place =
                    findAvailablePlaceForObject cellGrid (objectRow, instance) randomIntGeneratorWithSeed

                if place.IsSome then
                    placementFunction (objectRow, instance) place.Value
                    makeOccupied instance place.Value

                    inner (amountOfObjectsToBePlaced - 1)
                else
                    inner amountOfObjectsToBePlaced

        inner maximumAmountOfObjects
