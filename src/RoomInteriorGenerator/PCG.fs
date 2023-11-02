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
        match selectedObjectRow.PlacementRule with
        | Node(rule) ->
            match rule with
            | NodePlacementRule.AgainstTheWall -> (fun cell -> match cell with
                                                               | AgainstTheWall -> Some cell
                                                               | _ -> Option.None)
            | None ->
                (fun cell ->
                    match cell with
                    | NonOccupied | AgainstTheWall -> Some cell
                    | _ -> Option.None)
        | Leaf _ ->
            (fun cell ->
                match cell with
                | OccupiedForChildren -> Some cell
                | _ -> Option.None)

    let selectPotentiallyMatchingCells predicate =
        LimitedLengthArray(Array. predicate cellGrid.Data)

    let isCellMatching (instance: ObjectVariant<'Value>) (cell: Cell) (fittingCellsArray: LimitedLengthArray<Cell>) indexInFittingCellsArray =

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
                cellGrid.MakeOccupied(i, j)

    let parseLeafPlacementRule leafPlacementRule =
        match leafPlacementRule with
        | Leaf(rule) ->
            match rule with
            | LeftTo -> 0, 0, 1, 0
            | RightTo -> 0, 0, 0, 1
            | Behind -> 1, 0, 0, 0
            | InFrontOf -> 0, 1, 0, 0
            | Anywhere -> 1, 1, 1, 1
        | _ -> failwith "Leaf were expected here."

    let makeOccupiedForChildren (instance: ObjectVariant<'Value>) (cellRowIndex, cellColumnIndex) occupyRadius leafPlacement =
        let top, bottom, left, right = parseLeafPlacementRule leafPlacement

        for i in cellRowIndex - instance.freeCellsOnTheTop - occupyRadius * top .. cellRowIndex + instance.freeCellsOnTheBottom + occupyRadius * bottom do
            for j in cellColumnIndex - instance.freeCellsOnTheLeft - occupyRadius * left .. cellColumnIndex + instance.freeCellsOnTheRight + occupyRadius * right do
                if i < 0 || i > cellGrid.Width || j < 0 || j > cellGrid.Length || cellGrid.IsOccupied(i, j) then
                    ()
                else
                    cellGrid.MakeOccupiedForChildren(i, j)

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

                        let childRow, childInstance =
                            selectObjectToPlace (DataTable(objectRow.LeafsTable)) randomIntGeneratorWithSeed

                        printfn $"111111111111"

                        let maxColliderDimension =
                            max childInstance.freeCellsOnTheBottom childInstance.freeCellsOnTheRight
                            |> max childInstance.freeCellsOnTheLeft
                            |> max childInstance.freeCellsOnTheTop

                        let leafPlacement = childRow.PlacementRule

                        makeOccupiedForChildren instance place.Value maxColliderDimension leafPlacement

                        let childrenPlace =
                            findAvailablePlaceForObject cellGrid (childRow, childInstance) randomIntGeneratorWithSeed

                        if childrenPlace.IsSome then
                            placementFunction (childRow, childInstance) childrenPlace.Value
                            makeOccupied childInstance childrenPlace.Value
                            cellGrid.ClearOccupiedForChildrenCells

                    inner (amountOfObjectsToBePlaced - 1)
                else
                    inner amountOfObjectsToBePlaced

        inner maximumAmountOfObjects
