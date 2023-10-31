module RoomInteriorGenerator.Tests.Room

open Expecto

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ] }

module GenerateInterior =
    open Helper.Room

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

                  Expect.equal roomSampleFloat roomSampleFloatCopy "Room after furnishing did not change" ]
