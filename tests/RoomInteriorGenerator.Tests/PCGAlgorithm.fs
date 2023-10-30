module RoomInteriorGenerator.Tests.PCGAlgorithm

open Expecto
open FsCheck
open RoomInteriorGenerator.PCGAlgorithm
open RoomInteriorGenerator.DataTable
open Helper.DataTable
open Helper.Cell
open Helper.RandomGenerators
open RoomInteriorGenerator.Tests.Generators

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ]
        maxTest = 30 }

module ChooseObjectToPlace =
    [<Tests>]
    let tests =
        testList
            "choose object to place"
            [ testCase "Items selected from DataTable of size one and consisting of one ObjectInstance are the same"
              <| fun _ ->
                  let actualResult = chooseObjectToPlace dataTableOfLengthOne randomGeneratorSample
                  let expectedResult = chooseObjectToPlace dataTableOfLengthOne randomGeneratorSample

                  Expect.equal actualResult expectedResult "Generated items were expected to be equal"

              testPropertyWithConfig config "Items selected from DataTable of size one and consisting of one ObjectInstance are the same on generated int DataTable"
              <| fun (dataTableOfLengthOne: DataTableOfLengthOne<int>) ->
                  let actualResult =
                      chooseObjectToPlace dataTableOfLengthOne.Data randomGeneratorSample

                  let expectedResult =
                      chooseObjectToPlace dataTableOfLengthOne.Data randomGeneratorSample

                  Expect.equal actualResult expectedResult "Generated items were expected to be equal"

              testCase "A series of objects obtained from one generator with a seed match the subsequent ones obtained from a new generator with the same seed"
              <| fun _ ->
                  let generator1 = generateRandomIntNumber 10

                  let listOfGeneratedObjects1 =
                      List.init 5 (fun _ -> chooseObjectToPlace dataTableOfLengthThree generator1)

                  let generator2 = generateRandomIntNumber 10

                  let listOfGeneratedObjects2 =
                      List.init 5 (fun _ -> chooseObjectToPlace dataTableOfLengthThree generator2)

                  Expect.equal listOfGeneratedObjects1 listOfGeneratedObjects2 "Generated lists were expected to be equal"

              testPropertyWithConfig config "A series of objects obtained from one generator with a seed match the subsequent ones obtained from a new generator with the same seed property test"
              <| fun (lengthOfList: PositiveInt) (seed: int) (dataTable: DataTable<int>) ->
                  let generator1 = generateRandomIntNumber seed

                  let listOfGeneratedObjects1 =
                      List.init lengthOfList.Get (fun _ -> chooseObjectToPlace dataTable generator1)

                  let generator2 = generateRandomIntNumber seed

                  let listOfGeneratedObjects2 =
                      List.init lengthOfList.Get (fun _ -> chooseObjectToPlace dataTable generator2)

                  Expect.equal listOfGeneratedObjects1 listOfGeneratedObjects2 "Generated lists were expected to be equal" ]

module ChoosePlaceForObject =
    [<Tests>]
    let tests =
        testList
            "choose place for object"
            [ testCase "By selecting a place in an empty CellGrid we get None"
              <| fun _ ->
                  let actualResult =
                      choosePlaceForObject emptyCellGrid chosenObject randomGeneratorSample

                  Expect.equal actualResult Option.None "Chosen cell expected to be None"

              testPropertyWithConfig config "By selecting a place in an empty CellGrid we get None property test"
              <| fun (seed: int) (chosenObject: DataTableRow<obj> * ObjectInstance<obj>) ->
                  let actualResult =
                      generateRandomIntNumber seed |> choosePlaceForObject emptyCellGrid chosenObject

                  Expect.equal actualResult Option.None "Chosen cell expected to be None"

              testCase "By selecting a place in occupied CellGrid we get None"
              <| fun _ ->
                  let actualResult =
                      choosePlaceForObject occupiedCellGrid chosenObject randomGeneratorSample

                  Expect.equal actualResult Option.None "Chosen cell expected to be None"

              testPropertyWithConfig config "By selecting a place in occupied CellGrid we get None property test"
              <| fun (seed: int) (cellGridLength: PositiveInt) (cellGridWidth: PositiveInt) (chosenObject: DataTableRow<obj> * ObjectInstance<obj>) ->
                  let actualResult =
                      generateRandomIntNumber seed
                      |> choosePlaceForObject (makeOccupiedCellGrid cellGridLength.Get cellGridWidth.Get) chosenObject

                  Expect.equal actualResult Option.None "Chosen cell expected to be None" ]
