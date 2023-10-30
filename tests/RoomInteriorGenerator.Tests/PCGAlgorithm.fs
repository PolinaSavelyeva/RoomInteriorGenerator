module RoomInteriorGenerator.Tests.PCGAlgorithm

open Expecto
open FsCheck
open RoomInteriorGenerator.PCGAlgorithm
open RoomInteriorGenerator.DataTable
open Helper.DataTable
open Helper.RandomGenerators
open RoomInteriorGenerator.Tests.Generators

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ]
        maxTest = 50 }

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
