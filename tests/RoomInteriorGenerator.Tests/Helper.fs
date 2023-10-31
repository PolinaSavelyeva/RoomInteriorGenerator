module RoomInteriorGenerator.Tests.Helper

module DataTable =
    open RoomInteriorGenerator.DataTable

    let chairRow =
        DataTableRow("Chair", [ ObjectVariant("WhiteChair", 1, 1); ObjectVariant("BlackChair", 1, 1) ], Node None)

    let flowerpotRow =
        DataTableRow("Flowerpot", [ ObjectVariant("Flowerpot", 1, 1) ], Node None)

    let tableRow =
        DataTableRow("Table", [ ObjectVariant("DinnerTable", 2, 2); ObjectVariant("OfficeTable", 3, 2) ], Node None)

    let couchRow =
        DataTableRow("Couch", [ ObjectVariant("LongCouch", 1, 4) ], Node AgainstTheWall)

    let dataTableOfLengthOne = DataTable([| couchRow |])
    let dataTableOfLengthOneInstanceOne = DataTable([| flowerpotRow |])
    let dataTableOfLengthTwo = DataTable([| chairRow; tableRow |])
    let dataTableOfLengthThree = DataTable([| chairRow; tableRow; couchRow |])
    let chosenObject = couchRow, ObjectVariant("LongCouch", 1, 4)

module Cell =

    open RoomInteriorGenerator.Cell

    let makeAreaCellGrid areaLength areaWidth =

        let data =
            Array.init (areaLength * areaWidth) (fun index ->
                let i = index / areaLength
                let j = index % areaLength

                if i = 0 || i = areaWidth - 1 || j = 0 || j = areaLength - 1 then
                    Cell(AgainstTheWall, i, j)
                else
                    Cell(NonOccupied, i, j))

        CellGrid(data, areaLength, areaWidth)

    let makeCellGrid (cellsStatus: CellStatus) =
        fun areaLength areaWidth ->
            let data =
                Array.init (areaLength * areaWidth) (fun index -> Cell(cellsStatus, index / areaLength, index % areaLength))

            CellGrid(data, areaLength, areaWidth)

    let makeOccupiedCellGrid = makeCellGrid Occupied
    let makeNonOccupiedCellGrid = makeCellGrid NonOccupied

    let areaCellGrid = makeAreaCellGrid 101 142
    let nonOccupiedCellGridData = makeNonOccupiedCellGrid 300 77
    let occupiedCellGrid = makeOccupiedCellGrid 20 438
    let emptyCellGrid = makeAreaCellGrid 0 0

module Area =
    open RoomInteriorGenerator

    let widthSample1 = 56
    let lengthSample1 = 41

    let widthSample2 = 2
    let lengthSample2 = 2

    let areaSample1 = Array2D.init widthSample1 lengthSample1 (fun _ _ -> "None")
    let areaSample2 = Array2D.init widthSample2 lengthSample2 (fun _ _ -> "None")

    let areaSample1Copy = Array2D.copy areaSample1
    let areaSample2Copy = Array2D.copy areaSample2

    let mainAreaWithDataTableOfLength3 =
        Room(widthSample1, lengthSample1, 5, DataTable.dataTableOfLengthThree)

    let mainAreaWithDataTableOfLength1 =
        Room(widthSample2, lengthSample2, 5, DataTable.dataTableOfLengthOneInstanceOne)

    let areaFullOfFlowerpots =
        Array2D.init widthSample2 lengthSample2 (fun _ _ -> "Flowerpot")

    let placementFunction (areaToChange: string[,]) =

        fun (_: DataTable.DataTableRow<string>, instance: DataTable.ObjectVariant<string>) (cellRowIndex, cellColumnIndex) ->
            let diameterWidth = instance.ColliderWidth / 2
            let diameterLength = instance.ColliderLength / 2

            for i in cellColumnIndex - diameterLength .. cellColumnIndex + diameterLength do
                for j in cellRowIndex - diameterWidth .. cellRowIndex + diameterWidth do
                    areaToChange[i, j] <- instance.Instance

    let placementFunctionForSample1Area () = placementFunction areaSample1
    let placementFunctionForSample2Area () = placementFunction areaSample2

module RandomGenerators =

    let generateRandomIntNumber seed =
        let generator = System.Random(seed)
        fun lowerBound upperBound -> generator.Next(lowerBound, upperBound)

    let randomGeneratorSample = generateRandomIntNumber 99
