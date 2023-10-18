module RoomInteriorGenerator.Tests.DataTable

open Expecto
open FsCheck
open RoomInteriorGenerator.DataTable
open Helper.DataTable

module GettingElementFromDataTable =
    [<Tests>]
    let tests =
        testList
            "getting element from DataTable"
            [ testCase "Getting an element by an index greater than the length of the DataTable will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dataTableLength3[10] |> ignore) "Index out of the range"

              testCase "Getting an element by an index equal to the length of the DataTable will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dataTableLength2[2] |> ignore) "Index out of the range"

              testProperty "Getting an element by an index equal to the DataTable will cause an error on generated int DataTables"
              <| fun (dataTable: DataTable<int>) -> Expect.throws (fun _ -> dataTable[dataTable.Length] |> ignore) "Index out of the range"

              testProperty "Getting an element by an index equal to the DataTable will cause an error on generated string DataTables"
              <| fun (dataTable: DataTable<string>) -> Expect.throws (fun _ -> dataTable[dataTable.Length] |> ignore) "Index out of the range"

              testCase "Getting an negative index of the DataTable will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dataTableLength3[-1] |> ignore) "Index out of the range"

              testProperty "Getting an negative index of the DataTable will cause an error on generated int DataTables"
              <| fun (dataTable: DataTable<int>) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dataTable[negativeInt.Get] |> ignore) "Index out of the range"

              testProperty "Getting an negative index of the DataTable will cause an error on generated string DataTables"
              <| fun (dataTable: DataTable<string>) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dataTable[negativeInt.Get] |> ignore) "Index out of the range" ]
