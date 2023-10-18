module RoomInteriorGenerator.Tests.Generators

open FsCheck
open RoomInteriorGenerator.Cell
open RoomInteriorGenerator.DataTable

let cellGridGen =
    gen {
        let! length = Gen.choose (2, 100)
        let! width = Gen.choose (2, 100)
        let! data = Gen.arrayOfLength (length * width) (Gen.elements [ Cell(Occupied, 0, 0); Cell(NonOccupied, 0, 0); Cell(CellStatus.AgainstTheWall, 0, 0) ])
        return! Gen.constant (CellGrid(data, length, width))
    }

let intDataTableOfLengthOneGen =
    gen {
        let! colliderWidth = Gen.choose (2, 100)
        let! colliderLength = Gen.choose (2, 100)
        let objectInstance = ObjectInstance<int>(0, colliderWidth, colliderLength)

        let! rows =
            Gen.arrayOfLength
                1
                (Gen.elements [ DataTableRow("1", [ objectInstance ], Node AgainstTheWall); DataTableRow("2", [ objectInstance ], Node None); DataTableRow("3", [ objectInstance ], Node None) ])

        return! Gen.constant (DataTable<int>(rows))
    }

type Generators =
    static member CellGrid() = Arb.fromGen cellGridGen
    static member DataTable() = Arb.fromGen intDataTableOfLengthOneGen
