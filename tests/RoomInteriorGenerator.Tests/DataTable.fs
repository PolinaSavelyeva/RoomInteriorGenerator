module RoomInteriorGenerator.Tests.DataTable

open Expecto
open FsCheck
open Helper.DataTable
open RoomInteriorGenerator.Tests.Generators

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ] }

module GettingElementFromDataTable =
    [<Tests>]
    let tests =
        testList
            "getting element from DataTable"
            [ testCase "Getting an element by an index greater than the length of the DataTable will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dataTableOfLengthThree[10] |> ignore) "Index out of the range"

              testCase "Getting an element by an index equal to the length of the DataTable will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dataTableOfLengthTwo[2] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the DataTable will cause an error on generated int DataTables"
              <| fun (dataTable: IntDataTable) -> Expect.throws (fun _ -> dataTable.DataTable[dataTable.Length] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the DataTable will cause an error on generated bool DataTables"
              <| fun (dataTable: BoolDataTable) -> Expect.throws (fun _ -> dataTable.DataTable[dataTable.Length] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the DataTable will cause an error on generated string DataTables"
              <| fun (dataTable: StringDataTable) -> Expect.throws (fun _ -> dataTable.DataTable[dataTable.Length] |> ignore) "Index out of the range"

              testCase "Getting an negative index of the DataTable will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dataTableOfLengthThree[-1] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the DataTable will cause an error on generated int DataTables"
              <| fun (dataTable: IntDataTable) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dataTable.DataTable[negativeInt.Get] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the DataTable will cause an error on generated bool DataTables"
              <| fun (dataTable: BoolDataTable) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dataTable.DataTable[negativeInt.Get] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the DataTable will cause an error on generated string DataTables"
              <| fun (dataTable: StringDataTable) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dataTable.DataTable[negativeInt.Get] |> ignore) "Index out of the range"

              testCase "Getting an element by an 0 index of non-empty DataTable will return the element"
              <| fun _ -> Expect.equal (dataTableOfLengthThree[0]) chairRow "The element at index 0 should be chairRow" ]
