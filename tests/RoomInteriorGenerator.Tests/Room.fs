module RoomInteriorGenerator.Tests.Room

open Expecto
open RoomInteriorGenerator.Tests.Generators

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ] }

module GenerateInterior =
    open Helper.Room
    open Helper.DataTable
    open RoomInteriorGenerator

    [<Tests>]
    let tests =
        testList
            "generate interior"
            [ testCase "Room with generated interior with maximum amounts of objects = zero is the same as the previous one (string)"
              <| fun _ ->
                  mainRoomWithDataTableOfLength3.GenerateInterior 0 (placementFunctionForSample1Room ())

                  Expect.equal roomSample1 roomSample1Copy "Room after furnishing did not change"

              testPropertyWithConfig config "Room with generated interior with maximum amounts of objects = zero is the same as the previous one property test (string)"
              <| fun (room: string[,]) ->
                  let placementFunction = placementFunction room
                  let copyOfRoom = Array2D.copy room
                  mainRoomWithDataTableOfLength3.GenerateInterior 0 placementFunction

                  Expect.equal room copyOfRoom "Room after furnishing did not change"

              testCase "Room with generated interior with maximum amounts of objects = zero is the same as the previous one (int)"
              <| fun _ ->
                  mainRoomWithIntDataTable.GenerateInterior 0 (placementFunctionForIntSample ())

                  Expect.equal roomSampleInt roomSampleIntCopy "Room after furnishing did not change"

              testPropertyWithConfig config "Room with generated interior with maximum amounts of objects = zero is the same as the previous one property test (int)"
              <| fun (room: int[,]) ->
                  let placementFunction = placementFunction room
                  let copyOfRoom = Array2D.copy room
                  mainRoomWithIntDataTable.GenerateInterior 0 placementFunction

                  Expect.equal room copyOfRoom "Room after furnishing did not change"

              testCase "Room with generated interior with maximum amounts of objects = zero is the same as the previous one (float)"
              <| fun _ ->
                  mainRoomWithFloatDataTable.GenerateInterior 0 (placementFunctionForFloatSample ())

                  Expect.equal roomSampleFloat roomSampleFloatCopy "Room after furnishing did not change"

              testCase "Room with generated interior with maximum amounts of size 1 objects = length * width of room is filled by them"
              <| fun _ ->
                  mainRoomWithDataTableOfLength1.GenerateInterior (widthSample2 * lengthSample2) (placementFunctionForSampleFullOfFlowerpots ())

                  Expect.equal roomSample3 roomSampleFullOfFlowerpots "Room after furnishing filled with flowerpots"

              testPropertyWithConfig config "Room with generated interior with maximum amounts = 0 of room is empty property test"
              <| fun (roomArray: string[,]) (dataTable: StringDataTable) ->

                  let placementFunction = placementFunction roomArray
                  let copyOfRoomArray = Array2D.copy roomArray

                  let room =
                      Room(Array2D.length1 roomArray, Array2D.length2 roomArray, 10, dataTable.DataTable)

                  room.GenerateInterior 0 placementFunction

                  Expect.equal roomArray copyOfRoomArray "Room after furnishing did not change"

              testPropertyWithConfig config "Room with generated interior with maximum amounts of size 1 objects = length * width of room is filled by them property test"
              <| fun (roomArray: string[,]) ->

                  let placementFunction = placementFunction roomArray
                  let copyOfRoomArray = Array2D.copy roomArray

                  let room =
                      Room(Array2D.length1 roomArray, Array2D.length2 roomArray, 21, dataTableOfLengthOneInstanceOne)

                  room.GenerateInterior ((Array2D.length1 roomArray) * (Array2D.length2 roomArray)) placementFunction

                  Expect.equal roomArray copyOfRoomArray "Room after furnishing filled with flowerpots" ]
