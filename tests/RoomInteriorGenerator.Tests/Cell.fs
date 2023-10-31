module RoomInteriorGenerator.Tests.Cell

open Expecto
open FsCheck
open RoomInteriorGenerator.Cell
open Helper.Cell

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ] }

module GettingElementFromCellGrid =
    [<Tests>]
    let tests =
        testList
            "getting element from CellGrid"
            [ testCase "Getting an element by an index greater than the length of the CellGrid will cause an error"
              <| fun _ -> Expect.throws (fun _ -> cellGridOfRoom[200, 201] |> ignore) "Index out of the range"

              testCase "Getting an element by an index equal to the length of the CellGrid will cause an error"
              <| fun _ -> Expect.throws (fun _ -> cellGridOfRoom[cellGridOfRoom.Length, cellGridOfRoom.Width] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the CellGrid will cause an error on generated DataTables"
              <| fun (cellGrid: CellGrid) -> Expect.throws (fun _ -> cellGrid[cellGridOfRoom.Length, cellGridOfRoom.Width] |> ignore) "Index out of the range"

              testCase "Getting an negative index of the CellGrid will cause an error"
              <| fun _ -> Expect.throws (fun _ -> cellGridOfRoom[-1, 0] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the CellGrid will cause an error on generated DataTables"
              <| fun (cellGrid: CellGrid) (negativeIntRow: NegativeInt) (negativeIntColumn: NegativeInt) ->
                  Expect.throws (fun _ -> cellGrid[negativeIntRow.Get, negativeIntColumn.Get] |> ignore) "Index out of the range" ]
