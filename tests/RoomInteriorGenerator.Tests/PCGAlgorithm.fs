module RoomInteriorGenerator.Tests.PCGAlgorithm

open Expecto
open RoomInteriorGenerator.PCGAlgorithm
open RoomInteriorGenerator.DataTable
open Helper.DataTable
open Helper.RandomGenerators

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ] }

module ChooseObjectToPlace =
    [<Tests>]
    let tests =
        testList
            "choose object to place"
            [ testCase "Items selected from DataTable of size one and consisting of one ObjectInstance are the same"
              <| fun _ ->
                  let actualResult = chooseObjectToPlace dataTableLength1 randomGeneratorSample
                  let expectedResult = chooseObjectToPlace dataTableLength1 randomGeneratorSample

                  Expect.equal actualResult expectedResult "Generated items were expected to be equal"

              testPropertyWithConfig config "Items selected from DataTable of size one and consisting of one ObjectInstance are the same on generated int DataTable"
              <| fun (dataTableOfLengthOne: DataTable<int>) ->
                  let actualResult = chooseObjectToPlace dataTableOfLengthOne randomGeneratorSample
                  let expectedResult = chooseObjectToPlace dataTableOfLengthOne randomGeneratorSample

                  Expect.equal actualResult expectedResult "Generated items were expected to be equal" ]
