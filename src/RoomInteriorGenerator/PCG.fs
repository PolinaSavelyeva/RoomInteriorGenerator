module RoomInteriorGenerator.PCG

open DataTable
open Cell

let selectObjectToPlace (dataTable: DataTable<'Value>) randomIntGeneratorWithSeed =

    let objectToPlace = dataTable[randomIntGeneratorWithSeed 0 dataTable.Length]

    let objectToPlaceInstance =
        objectToPlace.Instances[(randomIntGeneratorWithSeed 0 objectToPlace.LengthOfInstancesArray)]

    objectToPlace, objectToPlaceInstance

let findAvailablePlaceForObject (cellGrid: CellGrid) (selectedObjectRow: DataTableRow<'Value>, selectedObjectInstance: ObjectVariant<'Value>) randomIntGeneratorWithSeed =

    let matchingCellsArray =
        let predicate =
            match selectedObjectRow.PlacementRule with
            | Node(rule) ->
                match rule with
                | NodePlacementRule.AgainstTheWall ->
                    (fun cell ->
                        match cell with
                        | AgainstTheWall -> true
                        | _ -> false)
                | None ->
                    (fun cell ->
                        match cell with
                        | NonOccupied
                        | AgainstTheWall -> true
                        | _ -> false)
            | Leaf _ ->
                (fun cell ->
                    match cell with
                    | OccupiedForChildren -> true
                    | _ -> false)

        let data =
            [| for i in 0 .. cellGrid.Width - 1 do
                   for j in 0 .. cellGrid.Length - 1 do
                       if predicate cellGrid[i, j] then
                           i, j |]

        LimitedLengthArray(data)


    let isCellMatching (cellRowIndex, cellColumnIndex) indexInMatchingCellsArray =

        // F# does not have break for loops
        let loop iStartIndex iFinishIndex jStartIndex jFinishIndex =

            let rec iLoop i =

                let rec jLoop j =
                    if j > jFinishIndex then
                        true
                    else if i < 0 || i >= cellGrid.Width || j < 0 || j >= cellGrid.Length || cellGrid.IsOccupied(i, j) then
                        matchingCellsArray.Delete indexInMatchingCellsArray
                        false
                    else
                        jLoop (j + 1)

                if i > iFinishIndex then true
                else if not (jLoop jStartIndex) then false
                else iLoop (i + 1)

            iLoop iStartIndex

        loop
            (cellRowIndex - selectedObjectInstance.freeCellsOnTheTop)
            (cellRowIndex + selectedObjectInstance.freeCellsOnTheBottom)
            (cellColumnIndex - selectedObjectInstance.freeCellsOnTheLeft)
            (cellColumnIndex + selectedObjectInstance.freeCellsOnTheRight)

    let selectCell (matchingCellsArray: LimitedLengthArray<int * int>) =

        // index in fittingCellsArray is not the same as cell.ColumnIndex or cell.RowIndex
        let indexInMatchingCellsArray =
            randomIntGeneratorWithSeed 0 matchingCellsArray.Length

        matchingCellsArray[indexInMatchingCellsArray], indexInMatchingCellsArray

    let rec inner () =
        if matchingCellsArray.IsEmpty then
            Option.None
        else
            let cellCoordinates, indexInFittingCellsArray = selectCell matchingCellsArray

            if isCellMatching cellCoordinates indexInFittingCellsArray then
                Some cellCoordinates
            else
                inner ()

    inner ()

let generateInterior (cellGrid: CellGrid) (dataTable: DataTable<'Value>) (maximumAmountOfObjects: int) placementFunction randomIntGeneratorWithSeed =

    let makeOccupied (instance: ObjectVariant<'Value>) (cellRowIndex, cellColumnIndex) =

        for i in cellRowIndex - instance.freeCellsOnTheTop .. cellRowIndex + instance.freeCellsOnTheBottom do
            for j in cellColumnIndex - instance.freeCellsOnTheLeft .. cellColumnIndex + instance.freeCellsOnTheRight do
                cellGrid.MakeOccupied(i, j)

    let makeOccupiedForChildren (parentInstance: ObjectVariant<'Value>) (cellRowIndex, cellColumnIndex) occupyRadius leafPlacementRule =

        let top, bottom, left, right =
            match leafPlacementRule with
            | Leaf(rule) ->
                match rule with
                | LeftTo -> 0, 0, 1, 0
                | RightTo -> 0, 0, 0, 1
                | Behind -> 1, 0, 0, 0
                | InFrontOf -> 0, 1, 0, 0
                | Anywhere -> 1, 1, 1, 1
            | _ -> failwith "Leaf were expected as input type."

        for i in cellRowIndex - parentInstance.freeCellsOnTheTop - occupyRadius * top .. cellRowIndex + parentInstance.freeCellsOnTheBottom + occupyRadius * bottom do
            for j in cellColumnIndex - parentInstance.freeCellsOnTheLeft - occupyRadius * left .. cellColumnIndex + parentInstance.freeCellsOnTheRight + occupyRadius * right do
                if i < 0 || i > cellGrid.Width || j < 0 || j > cellGrid.Length || cellGrid.IsOccupied(i, j) then
                    ()
                else
                    cellGrid.MakeOccupiedForChildren(i, j)

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
                let leafsTable = objectRow.LeafsTable

                if leafsTable.IsSome && amountOfObjectsToBePlaced <> 1 then
                    let childRow, childInstance =
                        selectObjectToPlace (DataTable(leafsTable.Value)) randomIntGeneratorWithSeed

                    let maxColliderDimension =
                        max childInstance.freeCellsOnTheBottom childInstance.freeCellsOnTheRight
                        |> max childInstance.freeCellsOnTheLeft
                        |> max childInstance.freeCellsOnTheTop

                    let leafPlacementRule = childRow.PlacementRule

                    makeOccupiedForChildren instance place.Value (maxColliderDimension + 1) leafPlacementRule

                    let childrenPlace =
                        findAvailablePlaceForObject cellGrid (childRow, childInstance) randomIntGeneratorWithSeed

                    if childrenPlace.IsSome then
                        placementFunction (childRow, childInstance) childrenPlace.Value

                        makeOccupied childInstance childrenPlace.Value
                        cellGrid.ClearOccupiedForChildrenCells

                        inner (amountOfObjectsToBePlaced - 2)
                    else
                        inner (amountOfObjectsToBePlaced - 1)

                else
                    inner (amountOfObjectsToBePlaced - 1)
            else
                inner amountOfObjectsToBePlaced

    inner maximumAmountOfObjects
