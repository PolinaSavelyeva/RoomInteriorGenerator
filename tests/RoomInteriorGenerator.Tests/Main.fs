namespace RoomInteriorGenerator.Tests

open Helper.Room

module ExpectoTemplate =

    open RoomInteriorGenerator.DataTable
    open RoomInteriorGenerator
    open System.IO

    [<EntryPoint>]
    let main argv =
        let width = 100
        let length = 100

        let area = Array2D.init width length (fun _ _ -> "None")

        // let placementFunction (roomToChange: 'Value[,]) =
        //
        //     fun (_: DataTable.DataTableRow<'Value>, instance: DataTable.ObjectVariant<'Value>) (cellRowIndex, cellColumnIndex) ->
        //
        //         for i in cellRowIndex - instance.freeCellsOnTheTop .. cellRowIndex + instance.freeCellsOnTheBottom do
        //             for j in cellColumnIndex - instance.freeCellsOnTheLeft .. cellColumnIndex + instance.freeCellsOnTheRight do
        //                 roomToChange[i, j] <- "Collider " + instance.Instance
        //
        //         roomToChange[cellRowIndex, cellColumnIndex] <- instance.Instance

        let placementFunction = placementFunction area

        let chairLeftRow =
            DataTableRow("Chair", [| ObjectVariant("WhiteChair", 1, 1, 1, 1); ObjectVariant("BlackChair", 1, 1, 1, 1); ObjectVariant("OrangeChair", 1, 1, 1, 1) |], Leaf LeftTo, Option.None)

        let chairRightRow =
            DataTableRow("Chair", [| ObjectVariant("WhiteChair", 1, 1, 1, 1); ObjectVariant("BlackChair", 1, 1, 1, 1); ObjectVariant("OrangeChair", 1, 1, 1, 1) |], Leaf RightTo, Option.None)

        let chairBehindRow =
            DataTableRow("Chair", [| ObjectVariant("WhiteChair", 1, 1, 1, 1); ObjectVariant("BlackChair", 1, 1, 1, 1); ObjectVariant("OrangeChair", 1, 1, 1, 1) |], Leaf Behind, Option.None)

        let chairInFrontOfRow =
            DataTableRow("Chair", [| ObjectVariant("WhiteChair", 1, 1, 1, 1); ObjectVariant("BlackChair", 1, 1, 1, 1); ObjectVariant("OrangeChair", 1, 1, 1, 1) |], Leaf InFrontOf, Option.None)

        let tableRow =
            DataTableRow(
                "Table",
                [| ObjectVariant("DinnerTable", 2, 2, 2, 2); ObjectVariant("OfficeTable", 2, 2, 3, 3) |],
                Node None,
                Some [| chairLeftRow; chairRightRow; chairBehindRow; chairInFrontOfRow |]
            )

        let couchRow =
            DataTableRow("Couch", [| ObjectVariant("LongCouch", 3, 3, 0, 0) |], Node AgainstTheBottomWall, Option.None)

        let table = DataTable([| tableRow; tableRow; couchRow |])

        let mainArea = Room(length, width, 291, table)

        let maximumAmountOfObjects = 100

        mainArea.GenerateInterior maximumAmountOfObjects placementFunction

        let arrayToWrite = area

        let filePath = Path.Combine(__SOURCE_DIRECTORY__ + "\Area.txt")

        let columnWidths =
            [ for col in 0 .. length - 1 ->
                  seq { for row in 0 .. width - 1 -> area.[row, col] }
                  |> Seq.maxBy (fun s -> s.Length)
                  |> String.length ]

        let writeArray2DToFile (filePath: string) (array2D: string[,]) =
            use writer = new StreamWriter(filePath)

            for row in 0 .. width - 1 do


                writer.WriteLine()
                writer.Write("[")

                for col in 0 .. length - 1 do
                    writer.Write(("\"" + array2D.[row, col] + "\"").PadRight(columnWidths.[col] + 3))

                    if col <> length - 1 then
                        writer.Write(",")
                    else
                        writer.Write("],")

        writeArray2DToFile filePath arrayToWrite
        0
