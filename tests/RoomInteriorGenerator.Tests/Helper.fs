module RoomInteriorGenerator.Tests.Helper

module DataTable =
    open RoomInteriorGenerator.DataTable

    let chairRow =
        DataTableRow("Chair", [| ObjectVariant("WhiteChair", 1, 1, 1, 1); ObjectVariant("BlackChair", 1, 1, 1, 1) |], Node None)

    let flowerpotRow =
        DataTableRow("Flowerpot", [| ObjectVariant("Flowerpot", 1, 1, 1, 1) |], Node None)

    let tableRow =
        DataTableRow("Table", [| ObjectVariant("DinnerTable", 2, 2, 2, 2); ObjectVariant("OfficeTable", 2, 2, 3, 3) |], Node None)

    let couchRow =
        DataTableRow("Couch", [| ObjectVariant("LongCouch", 1, 1, 4, 4) |], Node AgainstTheWall)

    let intRow =
        DataTableRow("1", [| ObjectVariant(1, 1, 4, 1, 4); ObjectVariant(2, 1, 1, 1, 0) |], Node AgainstTheWall)

    let floatRow =
        DataTableRow("1.0", [| ObjectVariant(1.0, 1, 4, 1, 4); ObjectVariant(2.0, 1, 1, 1, 0) |], Node AgainstTheWall)

    let dataTableOfLengthOne = DataTable([| couchRow |])
    let dataTableOfLengthOneInstanceOne = DataTable([| flowerpotRow |])
    let dataTableOfLengthTwo = DataTable([| chairRow; tableRow |])
    let dataTableOfLengthThree = DataTable([| chairRow; tableRow; couchRow |])
    let intDataTable = DataTable([| intRow; intRow; intRow; intRow |])
    let floatDataTable = DataTable([| floatRow; floatRow |])

    let chosenObject = couchRow, ObjectVariant("LongCouch", 1, 4, 4, 1)

module Cell =

    open RoomInteriorGenerator.Cell

    let makeCellGridOfRoom roomLength roomWidth =

        let data =
            Array.init (roomLength * roomWidth) (fun index ->
                let i = index / roomLength
                let j = index % roomLength

                if i = 0 || i = roomWidth - 1 || j = 0 || j = roomLength - 1 then
                    Cell(AgainstTheWall, i, j)
                else
                    Cell(NonOccupied, i, j))

        CellGrid(data, roomLength, roomWidth)

    let makeCellGrid (cellsStatus: CellStatus) =
        fun roomLength roomWidth ->
            let data =
                Array.init (roomLength * roomWidth) (fun index -> Cell(cellsStatus, index / roomLength, index % roomLength))

            CellGrid(data, roomLength, roomWidth)

    let makeOccupiedCellGrid = makeCellGrid Occupied
    let makeNonOccupiedCellGrid = makeCellGrid NonOccupied

    let cellGridOfRoom = makeCellGridOfRoom 101 142
    let nonOccupiedCellGridData = makeNonOccupiedCellGrid 300 77
    let occupiedCellGrid = makeOccupiedCellGrid 20 438
    let emptyCellGrid = makeCellGridOfRoom 0 0

module Room =
    open RoomInteriorGenerator

    let widthSample1 = 56
    let lengthSample1 = 41

    let widthSample2 = 2
    let lengthSample2 = 2

    let roomSample1 = Array2D.init widthSample1 lengthSample1 (fun _ _ -> "None")
    let roomSample2 = Array2D.init widthSample2 lengthSample2 (fun _ _ -> "None")
    let roomSampleInt = Array2D.init widthSample2 lengthSample2 (fun _ _ -> 0)
    let roomSampleFloat = Array2D.init widthSample2 lengthSample2 (fun _ _ -> 0.0)

    let roomSample1Copy = Array2D.copy roomSample1
    let roomSample2Copy = Array2D.copy roomSample2
    let roomSampleIntCopy = Array2D.copy roomSampleInt
    let roomSampleFloatCopy = Array2D.copy roomSampleFloat

    let mainRoomWithDataTableOfLength3 =
        Room(widthSample1, lengthSample1, 5, DataTable.dataTableOfLengthThree)

    let mainRoomWithDataTableOfLength1 =
        Room(widthSample2, lengthSample2, 5, DataTable.dataTableOfLengthOneInstanceOne)

    let mainRoomWithIntDataTable =
        Room(widthSample2, lengthSample2, 5, DataTable.intDataTable)

    let mainRoomWithFloatDataTable =
        Room(widthSample2, lengthSample2, 5, DataTable.floatDataTable)

    let roomFullOfFlowerpots =
        Array2D.init widthSample2 lengthSample2 (fun _ _ -> "Flowerpot")

    let placementFunction (roomToChange: 'Value[,]) =

        fun (_: DataTable.DataTableRow<'Value>, instance: DataTable.ObjectVariant<'Value>) (cellRowIndex, cellColumnIndex) ->

            for i in cellColumnIndex - instance.freeCellsOnTheLeft .. cellColumnIndex + instance.freeCellsOnTheRight do
                for j in cellRowIndex - instance.freeCellsOnTheTop .. cellRowIndex + instance.freeCellsOnTheBottom do
                    roomToChange[i, j] <- instance.Instance

    let placementFunctionForSample1Room () = placementFunction roomSample1
    let placementFunctionForSample2Room () = placementFunction roomSample2
    let placementFunctionForIntSample () = placementFunction roomSampleInt
    let placementFunctionForFloatSample () = placementFunction roomSampleFloat

module RandomGenerators =

    let generateRandomIntNumber seed =
        let generator = System.Random(seed)
        fun lowerBound upperBound -> generator.Next(lowerBound, upperBound)

    let randomGeneratorSample = generateRandomIntNumber 99
