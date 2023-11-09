module RoomInteriorGenerator.Tests.DynamicLengthArray

open Helper.DynamicLengthArray
open RoomInteriorGenerator
open Expecto
open FsCheck
open RoomInteriorGenerator.Tests.Generators

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ] }

module GettingElementFromDynamicLengthArray =
    [<Tests>]
    let tests =
        testList
            "getting element from DynamicLengthArray"
            [ testCase "Getting an element by an index greater than the length of the DynamicLengthArray will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dynamicLengthArray[12] |> ignore) "Index out of the range"

              testCase "Getting an element by an index equal to the length of the DynamicLengthArray will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dynamicLengthArray[11] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the DynamicLengthArray will cause an error on generated int DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayInt) -> Expect.throws (fun _ -> dynamicLengthArray.Data[dynamicLengthArray.Data.Length] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the DynamicLengthArray will cause an error on generated bool DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayBool) -> Expect.throws (fun _ -> dynamicLengthArray.Data[dynamicLengthArray.Data.Length] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the DynamicLengthArray will cause an error on generated string DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayString) -> Expect.throws (fun _ -> dynamicLengthArray.Data[dynamicLengthArray.Data.Length] |> ignore) "Index out of the range"

              testCase "Getting an negative index of the DynamicLengthArray will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dynamicLengthArray[-1] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the DynamicLengthArray will cause an error on generated int DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayInt) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dynamicLengthArray.Data[negativeInt.Get] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the DynamicLengthArray will cause an error on generated bool DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayBool) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dynamicLengthArray.Data[negativeInt.Get] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the DynamicLengthArray will cause an error on generated string DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayString) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dynamicLengthArray.Data[negativeInt.Get] |> ignore) "Index out of the range"

              testCase "Getting an element by an 0 index of non-empty DynamicLengthArray will return the element"
              <| fun _ -> Expect.equal (dynamicLengthArray[0]) 0 "The element at index 0 should be 0" ]
