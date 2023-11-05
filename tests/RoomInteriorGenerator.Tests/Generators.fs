module RoomInteriorGenerator.Tests.Generators

open FsCheck
open RoomInteriorGenerator.Cell
open RoomInteriorGenerator.DataTable


let cellGen =
    gen {
        let! cellStatus = Gen.elements [ NonOccupied; Occupied; Cell.AgainstTheWall; Cell.OccupiedForChildren ]
        return! Gen.constant cellStatus
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

let intObjectVariantGen =
    gen {
        let! variant = Gen.choose (0, 100)
        let! freeCellsOnTheRight = Gen.choose (1, 4)
        let! freeCellsOnTheLeft = Gen.choose (1, 4)
        let! freeCellsOnTheTop = Gen.choose (1, 4)
        let! freeCellsOnTheBottom = Gen.choose (1, 4)

        return! Gen.constant (ObjectVariant(variant, freeCellsOnTheRight, freeCellsOnTheLeft, freeCellsOnTheTop, freeCellsOnTheBottom))
    }

let stringObjectVariantGen =
    gen {
        let! variant = Gen.elements [ "addf"; "bsdsd" ]
        let! freeCellsOnTheRight = Gen.choose (1, 4)
        let! freeCellsOnTheLeft = Gen.choose (1, 4)
        let! freeCellsOnTheTop = Gen.choose (1, 4)
        let! freeCellsOnTheBottom = Gen.choose (1, 4)

        return! Gen.constant (ObjectVariant(variant, freeCellsOnTheRight, freeCellsOnTheLeft, freeCellsOnTheTop, freeCellsOnTheBottom))
    }

let boolObjectVariantGen =
    gen {
        let! variant = Gen.elements [ true; false ]
        let! freeCellsOnTheRight = Gen.choose (1, 4)
        let! freeCellsOnTheLeft = Gen.choose (1, 4)
        let! freeCellsOnTheTop = Gen.choose (1, 4)
        let! freeCellsOnTheBottom = Gen.choose (1, 4)

        return! Gen.constant (ObjectVariant(variant, freeCellsOnTheRight, freeCellsOnTheLeft, freeCellsOnTheTop, freeCellsOnTheBottom))
    }

let rulesGen =
    gen {
        let! nodePlacementRule = Gen.elements [ NodePlacementRule.AgainstTheWall; None ]
        return! Gen.constant (Node nodePlacementRule)
    }

let intDataTableOfLengthOneRowGen =
    gen {
        let! variantsArray = Gen.arrayOfLength 1 intObjectVariantGen
        let! rules = rulesGen
        return! Gen.constant (DataTableRow("Name", Infinity, variantsArray, rules, Option.None))
    }

let stringDataTableOfLengthOneRowGen =
    gen {
        let! variantsArray = Gen.arrayOfLength 1 stringObjectVariantGen
        let! rules = rulesGen
        return! Gen.constant (DataTableRow("Name", Infinity, variantsArray, rules, Option.None))
    }

let boolDataTableOfLengthOneRowGen =
    gen {
        let! variantsArray = Gen.arrayOfLength 1 boolObjectVariantGen
        let! rules = rulesGen
        return! Gen.constant (DataTableRow("Name", Infinity, variantsArray, rules, Option.None))
    }

let intDataTableOfLengthOneGen =
    gen {
        let! rows = Gen.arrayOfLength 1 intDataTableOfLengthOneRowGen
        return! Gen.constant <| DataTableOfLengthOne(DataTable(rows))
    }

let intDataTableRowGen =
    gen {
        let! length = Gen.choose (2, 50)
        let! variantsArray = Gen.arrayOfLength length intObjectVariantGen
        let! rules = rulesGen
        return! Gen.constant (DataTableRow("Name", Infinity, variantsArray, rules, Option.None))
    }

let stringDataTableRowGen =
    gen {
        let! length = Gen.choose (2, 50)
        let! variantsArray = Gen.arrayOfLength length stringObjectVariantGen
        let! rules = rulesGen
        return! Gen.constant (DataTableRow("Name", Infinity, variantsArray, rules, Option.None))
    }

let boolDataTableRowGen =
    gen {
        let! length = Gen.choose (2, 50)
        let! variantsArray = Gen.arrayOfLength length boolObjectVariantGen
        let! rules = rulesGen
        return! Gen.constant (DataTableRow("Name", Infinity, variantsArray, rules, Option.None))
    }

let intDataTableGen =
    gen {
        let! length = Gen.choose (2, 50)
        let! rows = Gen.arrayOfLength length intDataTableRowGen
        return! Gen.constant (DataTable(rows))
    }

let stringDataTableGen =
    gen {
        let! length = Gen.choose (2, 50)
        let! rows = Gen.arrayOfLength length stringDataTableRowGen
        return! Gen.constant (DataTable(rows))
    }

let boolDataTableGen =
    gen {
        let! length = Gen.choose (2, 50)
        let! rows = Gen.arrayOfLength length boolDataTableRowGen
        return! Gen.constant (DataTable(rows))
    }

type IntDataTable =
    val DataTable: DataTable<int>
    new(data) = { DataTable = data }
    member this.Length = this.DataTable.Length

type StringDataTable =
    val DataTable: DataTable<string>
    new(data) = { DataTable = data }
    member this.Length = this.DataTable.Length

type BoolDataTable =
    val DataTable: DataTable<bool>
    new(data) = { DataTable = data }
    member this.Length = this.DataTable.Length

type Generators =
    static member CellGrid() = Arb.fromGen cellGridGen
    static member IntDataTable() = Arb.fromGen intDataTableGen
    static member StringDataTable() = Arb.fromGen stringDataTableGen
    static member BoolDataTable() = Arb.fromGen boolDataTableGen
    static member DataTableRow() = Arb.fromGen intDataTableRowGen
    static member ObjectVariant() = Arb.fromGen intObjectVariantGen
    static member DataTableOfLengthOne() = Arb.fromGen intDataTableOfLengthOneGen
