module RoomInteriorGenerator.Tests.Room

open Expecto
open FsCheck

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ] }

module MakeCellGrid =
    open RoomInteriorGenerator.Cell
    open Helper.Cell

    [<Tests>]
    let tests =
        testList
            "make cell grid"
            [ testCase "Getting an element by an index greater than the length of the CellGrid will cause an error"
              <| fun _ -> Expect.throws (fun _ -> cellGridOfRoom[200, 201] |> ignore) "Index out of the range"

              testCase "Getting an element by an index equal to the length of the CellGrid will cause an error"
              <| fun _ -> Expect.throws (fun _ -> cellGridOfRoom[cellGridOfRoom.Length, cellGridOfRoom.Width] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the CellGrid will cause an error on generated int DataTables"
              <| fun (cellGrid: CellGrid) -> Expect.throws (fun _ -> cellGrid[cellGridOfRoom.Length, cellGridOfRoom.Width] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the CellGrid will cause an error on generated string DataTables"
              <| fun (cellGrid: CellGrid) -> Expect.throws (fun _ -> cellGrid[cellGridOfRoom.Length, cellGridOfRoom.Width] |> ignore) "Index out of the range"

              testCase "Getting an negative index of the CellGrid will cause an error"
              <| fun _ -> Expect.throws (fun _ -> cellGridOfRoom[-1, 0] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the CellGrid will cause an error on generated int DataTables"
              <| fun (cellGrid: CellGrid) (negativeIntRow: NegativeInt) (negativeIntColumn: NegativeInt) ->
                  Expect.throws (fun _ -> cellGrid[negativeIntRow.Get, negativeIntColumn.Get] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the CellGrid will cause an error on generated string DataTables"
              <| fun (cellGrid: CellGrid) (negativeIntRow: NegativeInt) (negativeIntColumn: NegativeInt) ->
                  Expect.throws (fun _ -> cellGrid[negativeIntRow.Get, negativeIntColumn.Get] |> ignore) "Index out of the range" ]

module GenerateInterior =
    open Helper.Room

    [<Tests>]
    let tests =
        testList
            "generate interior"
            [ testCase "Room with generated interior with maximum amounts of objects = zero is the same as the previous one"
              <| fun _ ->
                  mainRoomWithDataTableOfLength3.GenerateInterior 0 (placementFunctionForSample1Room ())

                  Expect.equal roomSample1 roomSample1Copy "Room after furnishing did not change"

              testPropertyWithConfig config "Room with generated interior with maximum amounts of objects = zero is the same as the previous one property test"
              <| fun (room: string[,]) ->
                  let placementFunction = placementFunction room
                  let copyOfRoom = Array2D.copy room
                  mainRoomWithDataTableOfLength3.GenerateInterior 0 placementFunction

                  Expect.equal room copyOfRoom "Room after furnishing did not change" ]
