module RoomInteriorGenerator.Tests.PCG

open Expecto
open FsCheck
open RoomInteriorGenerator.PCG
open RoomInteriorGenerator.DataTable
open Helper.DataTable
open Helper.RandomGenerators

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ]
        maxTest = 20 }

module SelectObjectToPlace =
    open Generators

    [<Tests>]
    let tests =
        testList
            "select object to place"
            [ testCase "Items selected from DataTable of size one and consisting of one ObjectInstance are the same"
              <| fun _ ->
                  let actualResult = selectObjectToPlace dataTableOfLengthOne randomGeneratorSample
                  let expectedResult = selectObjectToPlace dataTableOfLengthOne randomGeneratorSample

                  Expect.equal actualResult expectedResult "Generated items were expected to be equal"

              testPropertyWithConfig config "Items selected from DataTable of size one and consisting of one ObjectInstance are the same on generated int DataTable"
              <| fun (dataTableOfLengthOne: DataTableOfLengthOne<int>) ->
                  let actualResult =
                      selectObjectToPlace dataTableOfLengthOne.Data randomGeneratorSample

                  let expectedResult =
                      selectObjectToPlace dataTableOfLengthOne.Data randomGeneratorSample

                  Expect.equal actualResult expectedResult "Generated items were expected to be equal"

              testCase "A series of objects obtained from one generator with a seed match the subsequent ones obtained from a new generator with the same seed"
              <| fun _ ->
                  let generator1 = generateRandomIntNumber 10

                  let listOfGeneratedObjects1 =
                      List.init 5 (fun _ -> selectObjectToPlace dataTableOfLengthThree generator1)

                  let generator2 = generateRandomIntNumber 10

                  let listOfGeneratedObjects2 =
                      List.init 5 (fun _ -> selectObjectToPlace dataTableOfLengthThree generator2)

                  Expect.equal listOfGeneratedObjects1 listOfGeneratedObjects2 "Generated lists were expected to be equal"

              testPropertyWithConfig config "A series of objects obtained from one generator with a seed match the subsequent ones obtained from a new generator with the same seed property test"
              <| fun (lengthOfList: PositiveInt) (seed: int) (dataTable: DataTable<int>) ->
                  let generator1 = generateRandomIntNumber seed

                  let listOfGeneratedObjects1 =
                      List.init lengthOfList.Get (fun _ -> selectObjectToPlace dataTable generator1)

                  let generator2 = generateRandomIntNumber seed

                  let listOfGeneratedObjects2 =
                      List.init lengthOfList.Get (fun _ -> selectObjectToPlace dataTable generator2)

                  Expect.equal listOfGeneratedObjects1 listOfGeneratedObjects2 "Generated lists were expected to be equal" ]

module FindAvailablePlaceForObject =
    open Helper.Cell

    [<Tests>]
    let tests =
        testList
            "find available place for object"
            [ testCase "By selecting a place in an empty CellGrid we get None"
              <| fun _ ->
                  let actualResult =
                      findAvailablePlaceForObject emptyCellGrid chosenObject randomGeneratorSample

                  Expect.equal actualResult Option.None "Chosen cell expected to be None"

              testPropertyWithConfig config "By selecting a place in an empty CellGrid we get None property test"
              <| fun (seed: int) (chosenObject: DataTableRow<obj> * ObjectVariant<obj>) ->
                  let actualResult =
                      generateRandomIntNumber seed |> findAvailablePlaceForObject emptyCellGrid chosenObject

                  Expect.equal actualResult Option.None "Chosen cell expected to be None"

              testCase "By selecting a place in occupied CellGrid we get None"
              <| fun _ ->
                  let actualResult =
                      findAvailablePlaceForObject occupiedCellGrid chosenObject randomGeneratorSample

                  Expect.equal actualResult Option.None "Chosen cell expected to be None"

              testPropertyWithConfig config "By selecting a place in occupied CellGrid we get None property test"
              <| fun (seed: int) (cellGridLength: PositiveInt) (cellGridWidth: PositiveInt) (chosenObject: DataTableRow<obj> * ObjectVariant<obj>) ->
                  let actualResult =
                      generateRandomIntNumber seed
                      |> findAvailablePlaceForObject (makeOccupiedCellGrid cellGridLength.Get cellGridWidth.Get) chosenObject

                  Expect.equal actualResult Option.None "Chosen cell expected to be None" ]

module GenerateInterior =
    open Helper.Cell
    open Helper.Room

    [<Tests>]
    let tests =
        testList
            "generate interior"
            [ testCase "Place with generated interior with maximum amounts of objects = zero is the same as the previous one"
              <| fun _ ->
                  generateInterior cellGridOfRoom dataTableOfLengthThree 0 (placementFunctionForSample2Room ()) randomGeneratorSample

                  Expect.equal roomSample2 roomSample2Copy "Place after furnishing did not change"

              testPropertyWithConfig config "Place with generated interior with maximum amounts of objects = zero is the same as the previous one property test"
              <| fun (room: string[,]) ->
                  let placementFunction = placementFunction room
                  let copyOfRoom = Array2D.copy room
                  generateInterior cellGridOfRoom dataTableOfLengthThree 0 placementFunction randomGeneratorSample

                  Expect.equal room copyOfRoom "Place after furnishing did not change" ]
