module RoomInteriorGenerator.Tests.PCG

open System
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

module selectObjectToPlace =
    open Generators

    [<Tests>]
    let tests =
        testList
            "choose object to place"
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

module ChoosePlaceForObject =
    open Helper.Cell

    [<Tests>]
    let tests =
        testList
            "choose place for object"
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
    open Helper.Area

    [<Tests>]
    let tests =
        testList
            "generate interior"
            [ testCase "Room with generated interior with maximum amounts of objects = zero is the same as the previous one"
              <| fun _ ->
                  mainAreaWithDataTableOfLength3.GenerateInterior 0 (placementFunctionForSample1Area ())

                  Expect.equal areaSample1 areaSample1Copy "Room after furnishing did not change"

              testPropertyWithConfig config "Room with generated interior with maximum amounts of objects = zero is the same as the previous one property test"
              <| fun (area: string[,]) ->
                  let copyArea = Array2D.copy area
                  mainAreaWithDataTableOfLength3.GenerateInterior 0 (placementFunctionForSample1Area ())

                  Expect.equal area copyArea "Room after furnishing did not change" ]
