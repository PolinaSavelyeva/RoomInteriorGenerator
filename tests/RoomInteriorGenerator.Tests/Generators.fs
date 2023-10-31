module RoomInteriorGenerator.Tests.Generators

open FsCheck
open RoomInteriorGenerator.Cell
open RoomInteriorGenerator.DataTable


let cellGen =
    gen {
        let! rowIndex = Gen.choose (2, 100)
        let! columnIndex = Gen.choose (2, 100)
        let! cellStatus = Gen.elements [ NonOccupied; Occupied; CellStatus.AgainstTheWall ]
        return! Gen.constant (Cell(cellStatus, rowIndex, columnIndex))
    }

let cellGridGen =
    gen {
        let! length = Gen.choose (2, 100)
        let! width = Gen.choose (2, 100)
        let! data = Gen.arrayOfLength (length * width) cellGen
        return! Gen.constant (CellGrid(data, length, width))
    }

type DataTableOfLengthOne<'Value> =
    val Data: DataTable<'Value>
    new(data) = { Data = data }

let intObjectInstanceGen =
    gen {
        let! instance = Gen.choose (0, 1000)
        let! colliderWidth = Gen.choose (1, 4)
        let! colliderLength = Gen.choose (1, 4)
        return! Gen.constant (ObjectVariant(instance, colliderLength, colliderWidth))
    }

let rulesGen =
    gen {
        let! nodePlacementRule = Gen.elements [ NodePlacementRule.AgainstTheWall; None ]
        return! Gen.constant (Node nodePlacementRule)
    }

let intDataTableOfLengthOneRowGen =
    gen {
        let! instancesList = Gen.listOfLength 1 intObjectInstanceGen
        let! rules = rulesGen
        return! Gen.constant (DataTableRow("InstanceName", instancesList, rules))
    }

let intDataTableOfLengthOneGen =
    gen {
        let! rows = Gen.arrayOfLength 1 intDataTableOfLengthOneRowGen
        return! Gen.constant <| DataTableOfLengthOne(DataTable(rows))
    }

let intDataTableRowGen =
    gen {
        let! length = Gen.choose (1, 1000)
        let! instancesList = Gen.listOfLength length intObjectInstanceGen
        let! rules = rulesGen
        return! Gen.constant (DataTableRow("InstanceName", instancesList, rules))
    }

let intDataTableGen =
    gen {
        let! length = Gen.choose (1, 1000)
        let! rows = Gen.arrayOfLength length intDataTableRowGen
        return! Gen.constant (DataTable(rows))
    }

type Generators =
    static member CellGrid() = Arb.fromGen cellGridGen
    static member DataTable() = Arb.fromGen intDataTableGen
    static member DataTableOfLengthOne() = Arb.fromGen intDataTableOfLengthOneGen
