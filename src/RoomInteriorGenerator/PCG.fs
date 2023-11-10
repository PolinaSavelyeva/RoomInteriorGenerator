module RoomInteriorGenerator.PCG

open DataTable
open Cell
open Helper
open System.Collections.Generic


/// <summary>
/// Selects an object to place based on a DataTable and a random integer generator with seed.
/// </summary>
/// <param name="dataTable">The DataTable containing objects and their variants</param>
/// <param name="randomIntGeneratorWithSeed">The random integer generator with seed</param>
/// <returns>
/// Tuple containing the selected object, its instance, the index in the DataTable,
/// and the index of the selected variant.
/// </returns>
let selectObjectToPlace (dataTable: DataTable<'Value>) randomIntGeneratorWithSeed =

    let dataTableObjectIndex = randomIntGeneratorWithSeed 0 dataTable.Length
    let objectToPlace = dataTable[dataTableObjectIndex]

    let dataTableObjectVariantIndex =
        randomIntGeneratorWithSeed 0 objectToPlace.LengthOfVariantsArray

    let objectToPlaceInstance = objectToPlace.Variants[dataTableObjectVariantIndex]

    objectToPlace, objectToPlaceInstance, dataTableObjectIndex, dataTableObjectVariantIndex

/// <summary>
/// Finds an available place for a selected object.
/// </summary>
/// <param name="cellGrid">The CellGrid representing the available cells for object placement</param>
/// <param name="selectedObjectRow">The DataTableRow representing the selected object</param>
/// <param name="selectedObjectInstance">The ObjectVariant representing the selected instance of the object</param>
/// <param name="randomIntGeneratorWithSeed">The random integer generator with seed</param>
/// <returns>
/// Option type representing either the coordinates of the available cell or None if no suitable cell is found.
/// </returns>
let findAvailablePlaceForObject (cellGrid: CellGrid) (selectedObjectRow: DataTableRow<'Value>, selectedObjectInstance: ObjectVariant<'Value>) randomIntGeneratorWithSeed =

    /// <summary>
    /// Generates an dynamic length array of cell coordinates that match the placement rule of the selected object.
    /// </summary>
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

        DynamicLengthArray(data)

    /// <summary>
    /// Checks if a cell at the specified coordinates is a matching cell based on the given criteria.
    /// </summary>
    /// <param name="cellRowIndex">The row index of the cell</param>
    /// <param name="cellColumnIndex">The column index of the cell</param>
    /// <param name="indexInMatchingCellsArray">The index in the array of matching cells</param>
    /// <returns>True if the cell matches the criteria; otherwise, false</returns>
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
            (cellRowIndex - selectedObjectInstance.FreeCellsOnTheTop)
            (cellRowIndex + selectedObjectInstance.FreeCellsOnTheBottom)
            (cellColumnIndex - selectedObjectInstance.FreeCellsOnTheLeft)
            (cellColumnIndex + selectedObjectInstance.FreeCellsOnTheRight)

    /// <summary>
    /// Selects a random cell from the given array of matching cells.
    /// </summary>
    /// <param name="matchingCellsArray">The array of matching cells to choose from</param>
    /// <returns>The selected cell coordinates and the index in the array</returns>
    let selectCell (matchingCellsArray: DynamicLengthArray<int * int>) =

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

/// <summary>
/// Generates room interior by placing objects from DataTable on a CellGrid using specified placement function.
/// </summary>
/// <param name="cellGrid">The CellGrid representing the available cells for object placement</param>
/// <param name="dataTable">The DataTable containing objects and their variants</param>
/// <param name="maximumAmountOfObjects">The maximum number of objects to be placed</param>
/// <param name="placementFunction">The function responsible for placing objects on the grid</param>
/// <param name="randomIntGeneratorWithSeed">The random integer generator with seed</param>
let generateInterior (cellGrid: CellGrid) (dataTable: DataTable<'Value>) (maximumAmountOfObjects: int) placementFunction randomIntGeneratorWithSeed =

    /// <summary>
    /// Marks the cells within the specified range as occupied.
    /// </summary>
    /// <param name="instance">The object variant instance</param>
    /// <param name="cellRowIndex">The row index of the starting cell</param>
    /// <param name="cellColumnIndex">The column index of the starting cell</param>
    let makeOccupied (instance: ObjectVariant<'Value>) (cellRowIndex, cellColumnIndex) =

        for i in cellRowIndex - instance.FreeCellsOnTheTop .. cellRowIndex + instance.FreeCellsOnTheBottom do
            for j in cellColumnIndex - instance.FreeCellsOnTheLeft .. cellColumnIndex + instance.FreeCellsOnTheRight do
                cellGrid.MakeOccupied(i, j)

    /// <summary>
    /// Marks the cells within a specified range around the parent cell as occupied for children.
    /// </summary>
    /// <param name="parentVariant">The parent object variant</param>
    /// <param name="parentCellRowIndex">The row index of the parent cell</param>
    /// <param name="parentCellColumnIndex">The column index of the parent cell</param>
    /// <param name="occupyRadius">The radius around the parent cell to mark as occupied for children</param>
    /// <param name="leafPlacementRule">The leaf placement rule specifying how children are placed around the parent</param>
    let makeOccupiedForChildren (parentVariant: ObjectVariant<'Value>) (parentCellRowIndex, parentCellColumnIndex) occupyRadius leafPlacementRule =

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

        for i in parentCellRowIndex - parentVariant.FreeCellsOnTheTop - occupyRadius * top .. parentCellRowIndex + parentVariant.FreeCellsOnTheBottom + occupyRadius * bottom do
            for j in parentCellColumnIndex - parentVariant.FreeCellsOnTheLeft - occupyRadius * left .. parentCellColumnIndex + parentVariant.FreeCellsOnTheRight + occupyRadius * right do
                if not (i < 0 || i >= cellGrid.Width || j < 0 || j >= cellGrid.Length || cellGrid.IsOccupied(i, j)) then
                    cellGrid.MakeOccupiedForChildren(i, j)

    let rec inner
        (stack: Stack<Option<ObjectVariant<'Value>> * Option<int * int> * DataTable<'Value>>)
        (currentParentObjectVariant: Option<ObjectVariant<'Value>>)
        (currentParentPlace: Option<int * int>)
        (currentDataTable: DataTable<'Value>)
        amountOfObjectsToBePlaced
        =

        if amountOfObjectsToBePlaced = 0 || dataTable.IsEmpty then
            ()
        elif currentDataTable.IsEmpty then

            currentDataTable.Restore

            let previousObjectInformation = stack.Pop()

            inner stack (first previousObjectInformation) (second previousObjectInformation) (third previousObjectInformation) amountOfObjectsToBePlaced
        else
            let objectRow, objectVariant, dataTableObjectIndex, dataTableVariantIndex =
                selectObjectToPlace currentDataTable randomIntGeneratorWithSeed

            if currentParentObjectVariant.IsSome then
                let maxColliderDimension =
                    max objectVariant.FreeCellsOnTheBottom objectVariant.FreeCellsOnTheRight
                    |> max objectVariant.FreeCellsOnTheLeft
                    |> max objectVariant.FreeCellsOnTheTop

                let leafPlacementRule = objectRow.PlacementRule

                makeOccupiedForChildren currentParentObjectVariant.Value currentParentPlace.Value (maxColliderDimension + 1) leafPlacementRule

            let place =
                findAvailablePlaceForObject cellGrid (objectRow, objectVariant) randomIntGeneratorWithSeed

            if currentParentObjectVariant.IsSome then
                cellGrid.CleanOccupiedForChildrenCells

            if place.IsSome then

                placementFunction (objectRow, objectVariant) place.Value
                makeOccupied objectVariant place.Value

                let leafsTable = objectRow.LeafsTable

                if leafsTable.IsSome then
                    stack.Push(currentParentObjectVariant, currentParentPlace, currentDataTable)
                    inner stack (Some objectVariant) place (DataTable(leafsTable.Value)) (amountOfObjectsToBePlaced - 1)
                else
                    inner stack currentParentObjectVariant currentParentPlace currentDataTable (amountOfObjectsToBePlaced - 1)
            else
                if objectRow.Variants.IsEmpty || objectRow.LengthOfVariantsArray = 1 then
                    currentDataTable.Delete dataTableObjectIndex
                else
                    objectRow.Variants.Delete dataTableVariantIndex

                inner stack currentParentObjectVariant currentParentPlace currentDataTable amountOfObjectsToBePlaced

    let stack = Stack()
    inner stack Option.None Option.None dataTable maximumAmountOfObjects
