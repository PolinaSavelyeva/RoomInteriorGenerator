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
              <| fun _ -> Expect.throws (fun _ -> dynamicLengthArraySample1[12] |> ignore) "Index out of the range"

              testCase "Getting an element by an index equal to the length of the DynamicLengthArray will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dynamicLengthArraySample1[11] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the DynamicLengthArray will cause an error on generated int DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayInt) -> Expect.throws (fun _ -> dynamicLengthArray.Data[dynamicLengthArray.Data.Length] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the DynamicLengthArray will cause an error on generated bool DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayBool) -> Expect.throws (fun _ -> dynamicLengthArray.Data[dynamicLengthArray.Data.Length] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an element by an index equal to the DynamicLengthArray will cause an error on generated string DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayString) -> Expect.throws (fun _ -> dynamicLengthArray.Data[dynamicLengthArray.Data.Length] |> ignore) "Index out of the range"

              testCase "Getting an negative index of the DynamicLengthArray will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dynamicLengthArraySample1[-1] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the DynamicLengthArray will cause an error on generated int DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayInt) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dynamicLengthArray.Data[negativeInt.Get] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the DynamicLengthArray will cause an error on generated bool DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayBool) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dynamicLengthArray.Data[negativeInt.Get] |> ignore) "Index out of the range"

              testPropertyWithConfig config "Getting an negative index of the DynamicLengthArray will cause an error on generated string DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayString) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dynamicLengthArray.Data[negativeInt.Get] |> ignore) "Index out of the range"

              testCase "Getting an element by an 0 index of non-empty DynamicLengthArray will return the element"
              <| fun _ -> Expect.equal (dynamicLengthArraySample1[0]) 0 "The element at index 0 should be 0" ]

module DeleteElementFromDynamicLengthArray =
    [<Tests>]
    let tests =
        testList
            "delete element from DynamicLengthArray"
            [ testCase "Deleting an element by an index greater than the length of the DynamicLengthArray will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dynamicLengthArraySample2.Delete 12) "Index out of the range"

              testCase "Deleting an element by an index equal to the length of the DynamicLengthArray will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dynamicLengthArraySample2.Delete 11) "Index out of the range"

              testPropertyWithConfig config "Deleting an element by an index equal to the DynamicLengthArray will cause an error on generated int DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayInt) -> Expect.throws (fun _ -> dynamicLengthArray.Data.Delete dynamicLengthArray.Data.Length) "Index out of the range"

              testPropertyWithConfig config "Deleting an element by an index equal to the DynamicLengthArray will cause an error on generated bool DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayBool) -> Expect.throws (fun _ -> dynamicLengthArray.Data.Delete dynamicLengthArray.Data.Length) "Index out of the range"

              testPropertyWithConfig config "Deleting an element by an index equal to the DynamicLengthArray will cause an error on generated string DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayString) -> Expect.throws (fun _ -> dynamicLengthArray.Data.Delete dynamicLengthArray.Data.Length) "Index out of the range"

              testCase "Deleting an negative index of the DynamicLengthArray will cause an error"
              <| fun _ -> Expect.throws (fun _ -> dynamicLengthArraySample2.Delete -1) "Index out of the range"

              testPropertyWithConfig config "Deleting an negative index of the DynamicLengthArray will cause an error on generated int DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayInt) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dynamicLengthArray.Data.Delete negativeInt.Get) "Index out of the range"

              testPropertyWithConfig config "Deleting an negative index of the DynamicLengthArray will cause an error on generated bool DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayBool) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dynamicLengthArray.Data.Delete negativeInt.Get) "Index out of the range"

              testPropertyWithConfig config "Deleting an negative index of the DynamicLengthArray will cause an error on generated string DynamicLengthArray"
              <| fun (dynamicLengthArray: DynamicLengthArrayString) (negativeInt: NegativeInt) -> Expect.throws (fun _ -> dynamicLengthArray.Data.Delete negativeInt.Get) "Index out of the range"

              testCase "Deleting an element by an 0 index of non-empty DynamicLengthArray will work correctly"
              <| fun _ -> Expect.equal (dynamicLengthArraySample2.Delete 0) () "The element at index 0 delete successfully" ]
