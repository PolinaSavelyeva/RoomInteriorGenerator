---
title: Getting Started
category: Tutorials
categoryindex: 1
index: 1
---

# Getting Started with ImageProcessing

---

## Preparing

### Requirements

Make sure the following requirements are installed on your system:

- [dotnet SDK](https://www.microsoft.com/net/download/core) 3.0 or higher (recommended 6.0+),
- [Mono](http://www.mono-project.com/) if you're on Linux or macOS
  or
- [VSCode Dev Container](https://code.visualstudio.com/docs/remote/containers).

### Template
To find more building and running options take a look at the [MiniScaffold](https://github.com/TheAngryByrd/MiniScaffold) template.

### Package Adding
Download package via [NuGet](https://www.nuget.org/packages/RoomInteriorGenerator/).

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

Take a look at the [Time-Reactor-Game](https://github.com/RuslanBeresnev/Time-Reactor-Game) to see all the steps of package implementation.

