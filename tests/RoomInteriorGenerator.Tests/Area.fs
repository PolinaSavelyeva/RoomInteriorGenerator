module RoomInteriorGenerator.Tests.Area

open Expecto
open FsCheck
open RoomInteriorGenerator.Cell
open Helper.Cell

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ] }

module MakeCellGrid =
    [<Tests>]
    let tests =
        testList
            "make cell grid"
            [ testCase "Getting an element by an index greater than the length of the CellGrid will cause an error"
              <| fun _ -> Expect.throws (fun _ -> areaCellGrid[200, 201] |> ignore) "Index out of the range"

              testCase "Getting an element by an index equal to the length of the CellGrid will cause an error"
              <| fun _ -> Expect.throws (fun _ -> areaCellGrid[areaCellGrid.Length, areaCellGrid.Width] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the CellGrid will cause an error on generated int DataTables"
              <| fun (cellGrid: CellGrid) -> Expect.throws (fun _ -> cellGrid[areaCellGrid.Length, areaCellGrid.Width] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the CellGrid will cause an error on generated string DataTables"
              <| fun (cellGrid: CellGrid) -> Expect.throws (fun _ -> cellGrid[areaCellGrid.Length, areaCellGrid.Width] |> ignore) "Index out of the range"

              testCase "Getting an negative index of the CellGrid will cause an error"
              <| fun _ -> Expect.throws (fun _ -> areaCellGrid[-1, 0] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the CellGrid will cause an error on generated int DataTables"
              <| fun (cellGrid: CellGrid) (negativeIntRow: NegativeInt) (negativeIntColumn: NegativeInt) ->
                  Expect.throws (fun _ -> cellGrid[negativeIntRow.Get, negativeIntColumn.Get] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the CellGrid will cause an error on generated string DataTables"
              <| fun (cellGrid: CellGrid) (negativeIntRow: NegativeInt) (negativeIntColumn: NegativeInt) ->
                  Expect.throws (fun _ -> cellGrid[negativeIntRow.Get, negativeIntColumn.Get] |> ignore) "Index out of the range" ]
