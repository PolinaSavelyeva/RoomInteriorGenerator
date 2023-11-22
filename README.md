# RoomInteriorGenerator

[![NuGet Badge](https://buildstats.info/nuget/RoomInteriorGenerator)](https://www.nuget.org/packages/RoomInteriorGenerator/) 

RoomInteriorGenerator is a .NET library that helps developers to create game environments. It allows you to focus only on defining different objects and hierarchical rules between them instead of an arragment algorithm.

##  Documentation

See the [GitHub pages](https://polinasavelyeva.github.io/RoomInteriorGenerator/) for the API reference and more information.

## Overview

The generation process is based on several structures:

- **DataTable**: Lists objects and the rules between them. Consists of several "columns": object name, object variants to choose from, rules describing the behaviour of the object and the expected amount of free space around it, and possible child objects that can be placed around the parent.
- **Cell**: Cell defines the smallest possible unit of a room for arranging furniture. It has several basic states such as Occupied, NonOccupied or AgainstTheWall, which can be extended to give a more accurate furnishing result.
- **CellGrid**: Describes the available room space using cells and their positions.
ngth.
- **Room**: Encapsulates a room using its width, height, floor number as seed for random generator, placement function and data table. 

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

![Plot](https://github.com/PolinaSavelyeva/RoomInteriorGenerator/blob/sample/Samples/withoutcorners.png)


##  Unity Example

First, add the RoomInteriorGenerator package to your Unity project when you can open it in your desired script. I fill the data table in the inspector and add the current script to the new GameObject. GameObject should be in the top left corner of your room. GameObject will be the coordinate origin for all furniture placement, so make sure you set the x and z axis directions correctly. Also, all the prefabs you want to place should have (0,0,0) position and 0 degrees y-rotation (depending on your GameObject directions this may be different). Be sure to check the object's pivot point, usually it should be in the centre of your object. This will help you to get the most expected result.

![Inspector](https://github.com/PolinaSavelyeva/RoomInteriorGenerator/blob/sample/Samples/Inspector.png)

The main body of my script looks like this:

```csharp
private void Start()
{
    var rows = dataTable.Select(row => row.ToDataTableRow()).ToArray();
    var dataTableFs = new DataTable.DataTable<GameObject>(rows);
    var room = new Room<GameObject>(roomLength, roomWidth, floorNumber, dataTableFs);
    var tupleFunction =
        FuncConvert.ToFSharpFunc<Tuple<DataTable.DataTableRow<GameObject>, DataTable.ObjectVariant<GameObject>, int, int>>(
                t => PlaceObject(t.Item1, t.Item2, t.Item3, t.Item4));
    var placementFunction = FuncConvert.FuncFromTupled(tupleFunction);

    room.GenerateInterior(maximumAmountOfObjects, placementFunction);
}
```

The final result:

![Room](https://github.com/PolinaSavelyeva/RoomInteriorGenerator/blob/sample/Samples/Room.png)

Take a look at the [Time-Reactor](https://github.com/RuslanBeresnev/Time-Reactor-Game) game to see all the steps of package implementation.

## Requirements

Make sure the following requirements are installed on your system:

- [dotnet SDK](https://www.microsoft.com/net/download/core) 3.0 or higher (recommended 6.0+),
- [Mono](http://www.mono-project.com/) if you're on Linux or macOS
or
- [VSCode Dev Container](https://code.visualstudio.com/docs/remote/containers).

### Template
To find more building and running options take a look at the [MiniScaffold](https://github.com/TheAngryByrd/MiniScaffold) template.
