module RoomInteriorGenerator.Tests.Helper

module DataTable =

    open RoomInteriorGenerator.DataTable

    let chairRow =
        DataTableRow("Chair", [ ObjectInstance("WhiteChair", 1, 1); ObjectInstance("BlackChair", 1, 1) ], Node None)

    let tableRow =
        DataTableRow("Table", [ ObjectInstance("DinnerTable", 2, 2); ObjectInstance("OfficeTable", 3, 2) ], Node None)

    let couchRow =
        DataTableRow("Couch", [ ObjectInstance("LongCouch", 1, 4) ], Node AgainstTheWall)

    let dataTableOfLengthOne = DataTable([| couchRow |])
    let dataTableOfLengthTwo = DataTable([| chairRow; tableRow |])
    let dataTableOfLengthThree = DataTable([| chairRow; tableRow; couchRow |])

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

    let makeNonOccupiedCellGrid areaLength areaWidth =

        let data =
            Array.init (areaLength * areaWidth) (fun index -> Cell(NonOccupied, index / areaLength, index % areaLength))

        CellGrid(data, areaLength, areaWidth)

    let areaCellGridData = makeAreaCellGrid 101 142
    let nonOccupiedCellGridData = makeAreaCellGrid 300 77

module RandomGenerators =

    let generateRandomIntNumber seed =
        let generator = System.Random(seed)
        fun lowerBound upperBound -> generator.Next(lowerBound, upperBound)

    let randomGeneratorSample = generateRandomIntNumber 99
