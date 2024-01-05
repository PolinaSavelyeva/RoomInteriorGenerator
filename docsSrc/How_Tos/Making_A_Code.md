---
title: How To make a code
category: How To Guides
categoryindex: 2
index: 1
---

# How-To make your own RoomInteriorGenerator code

---

Take a look at a simple code example where we want chairs to stand around the table and couches to stand near the bottom wall. First, we'll create data table rows for all the furniture we want to use, and then create the data table itself:

```fsharp
let chairLeftRow =
    DataTableRow("Chair", [| ObjectVariant("WhiteChair", 1, 1, 1, 1); ObjectVariant("BlackChair", 1, 1, 1, 1); ObjectVariant("OrangeChair", 1, 1, 1, 1) |], Leaf LeftTo, Option.None)

let couchRow =
    DataTableRow("Couch", [| ObjectVariant("LongCouch", 3, 3, 0, 0) |], Node AgainstTheBottomWall, Option.None)
    
let tableRow =
     DataTableRow( "Table", [| ObjectVariant("DinnerTable", 2, 2, 2, 2); ObjectVariant("OfficeTable", 2, 2, 3, 3) |],  Node None, Some [| chairLeftRow; chairRightRow; chairBehindRow; chairInFrontOfRow |])

let table = DataTable([| chairLeftRow; tableRow; couchRow |])
```

Our placement function will replace the array values with the selected object variant name:


```fsharp
let arrayToChange = Array2D.init width length (fun _ _ -> "None")

let placementFunction =

    fun (_: DataTable.DataTableRow<'Value>) (instance: DataTable.ObjectVariant<'Value>) cellRowIndex cellColumnIndex ->
        for i in cellRowIndex - instance.FreeCellsOnTheTop .. cellRowIndex + instance.FreeCellsOnTheBottom do
            for j in cellColumnIndex - instance.FreeCellsOnTheLeft .. cellColumnIndex + instance.FreeCellsOnTheRight do
                arrayToChange[i, j] <- instance.Variant

let room = Room(length, width, 291, table)    
room.GenerateInterior maximumAmountOfObjects placementFunction
```

After the visualising of the array, we have the following result, where tables are green and blue, chairs are black, white and orange and couches are red:

![Plot](https://raw.githubusercontent.com/PolinaSavelyeva/RoomInteriorGenerator/sample/Samples/withoutcorners.png)


