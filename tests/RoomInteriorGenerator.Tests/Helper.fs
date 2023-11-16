module RoomInteriorGenerator.Tests.Helper

open RoomInteriorGenerator

module DynamicLengthArray =

    let dynamicLengthArraySample1 = DynamicLengthArray(Array.init 11 id)
    let dynamicLengthArraySample2 = DynamicLengthArray(Array.init 5 id)

module DataTable =
    open RoomInteriorGenerator.DataTable

    let chairRow =
        DataTableRow("Chair", [| ObjectVariant("WhiteChair", 1, 1, 1, 1); ObjectVariant("BlackChair", 1, 1, 1, 1) |], Node None, Option.None)

    let flowerpotRow =
        DataTableRow("Flowerpot", [| ObjectVariant("Flowerpot", 0, 0, 0, 0) |], Node None, Option.None)

    let tableRow =
        DataTableRow("Table", [| ObjectVariant("DinnerTable", 2, 2, 2, 2); ObjectVariant("OfficeTable", 2, 2, 3, 3) |], Node None, Option.None)

    let couchRow =
        DataTableRow("Couch", [| ObjectVariant("LongCouch", 1, 1, 4, 4) |], Node AgainstTheLeftWall, Option.None)

    let intRow =
        DataTableRow("1", [| ObjectVariant(1, 1, 4, 1, 4); ObjectVariant(2, 1, 1, 1, 0) |], Node AgainstTheLeftWall, Option.None)

    let floatRow =
        DataTableRow("1.0", [| ObjectVariant(1.0, 1, 4, 1, 4); ObjectVariant(2.0, 1, 1, 1, 0) |], Node AgainstTheLeftWall, Option.None)

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

        let (data: array<Cell>) =
            Array.init (roomLength * roomWidth) (fun index ->
                let i = index / roomLength
                let j = index % roomLength

                if j = 0 && i <> roomWidth - 1 && i <> 0 then
                    AgainstTheLeftWall
                elif i = 0 && j <> 0 && j <> roomLength - 1 then
                    AgainstTheTopWall
                elif j = roomLength - 1 && i <> 0 && i <> roomWidth - 1 then
                    AgainstTheRightWall
                elif i = roomWidth - 1 && j <> 0 && j <> roomLength - 1 then
                    AgainstTheBottomWall
                elif
                    (i = 0 && j = 0)
                    || (i = 0 && j = roomLength - 1)
                    || (i = roomWidth - 1 && j = roomLength - 1)
                    || (i = roomWidth - 1 && j = 0)
                then
                    Corner
                else
                    NonOccupied)

        CellGrid(data, roomLength, roomWidth)

    let makeCellGrid (cellsStatus: Cell) =
        fun roomLength roomWidth ->
            let data = Array.init (roomLength * roomWidth) (fun _ -> cellsStatus)

            CellGrid(data, roomLength, roomWidth)

    let makeOccupiedCellGrid = makeCellGrid Occupied
    let makeNonOccupiedCellGrid = makeCellGrid NonOccupied

    let cellGridOfRoom = makeCellGridOfRoom 101 142
    let cellGridOfRoomForFlowerpots = makeCellGridOfRoom 4 4
    let nonOccupiedCellGridData = makeNonOccupiedCellGrid 300 77
    let occupiedCellGrid = makeOccupiedCellGrid 20 438
    let emptyCellGrid = makeCellGridOfRoom 0 0

module Room =

    let widthSample1 = 56
    let lengthSample1 = 41

    let widthSample2 = 4
    let lengthSample2 = 4

    let roomSample1 = Array2D.init widthSample1 lengthSample1 (fun _ _ -> "None")
    let roomSample2 = Array2D.init widthSample2 lengthSample2 (fun _ _ -> "None")
    let roomSample3 = Array2D.init widthSample2 lengthSample2 (fun _ _ -> "None")
    let roomSampleInt = Array2D.init widthSample2 lengthSample2 (fun _ _ -> 0)
    let roomSampleFloat = Array2D.init widthSample2 lengthSample2 (fun _ _ -> 0.0)

    let roomSampleFullOfFlowerpots =
        Array2D.init widthSample2 lengthSample2 (fun _ _ -> "Flowerpot")

    let roomSample1Copy = Array2D.copy roomSample1
    let roomSample2Copy = Array2D.copy roomSample2
    let roomSampleIntCopy = Array2D.copy roomSampleInt
    let roomSampleFloatCopy = Array2D.copy roomSampleFloat

    let mainRoomWithDataTableOfLength3 =
        Room(lengthSample1, widthSample1, 9, DataTable.dataTableOfLengthThree)

    let mainRoomWithDataTableOfLength1 =
        Room(lengthSample2, widthSample2, 7, DataTable.dataTableOfLengthOneInstanceOne)

    let mainRoomWithIntDataTable =
        Room(lengthSample2, widthSample2, 1, DataTable.intDataTable)

    let mainRoomWithFloatDataTable =
        Room(lengthSample2, widthSample2, 2, DataTable.floatDataTable)

    let placementFunction (roomToChange: 'Value[,]) =

        fun (_: DataTable.DataTableRow<'Value>) (instance: DataTable.ObjectVariant<'Value>) cellRowIndex cellColumnIndex ->

            for i in cellRowIndex - instance.FreeCellsOnTheTop .. cellRowIndex + instance.FreeCellsOnTheBottom do
                for j in cellColumnIndex - instance.FreeCellsOnTheLeft .. cellColumnIndex + instance.FreeCellsOnTheRight do
                    roomToChange[i, j] <- instance.Variant

    let placementFunctionForSample1Room () = placementFunction roomSample1
    let placementFunctionForSample2Room () = placementFunction roomSample2
    let placementFunctionForIntSample () = placementFunction roomSampleInt
    let placementFunctionForFloatSample () = placementFunction roomSampleFloat
    let placementFunctionForSampleFullOfFlowerpots () = placementFunction roomSample3

module RandomGenerators =

    let generateRandomIntNumber seed =
        let generator = System.Random(seed)
        fun lowerBound upperBound -> generator.Next(lowerBound, upperBound)

    let randomGeneratorSample = generateRandomIntNumber 99
