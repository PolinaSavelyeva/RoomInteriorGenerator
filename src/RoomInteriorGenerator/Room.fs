namespace RoomInteriorGenerator

open DataTable
open Cell
open PCG
open System

type Room<'Value> =
    // Room's lower left corner must be at (0,0)
    val Length: int
    val Width: int
    val FloorNumber: int
    val AreaDataTable: DataTable<'Value>

    new(length, width, floorNumber, areaDataTable) =
        { Length = length
          Width = width
          FloorNumber = floorNumber
          AreaDataTable = areaDataTable }

    member private this.GenerateCellGrid =
        let makeCellGrid areaLength areaWidth =

            let data =
                Array.init (areaLength * areaWidth) (fun index ->
                    let i = index / areaLength
                    let j = index % areaLength

                    if i = 0 || i = areaWidth - 1 || j = 0 || j = areaLength - 1 then
                        Cell(CellStatus.AgainstTheWall, i, j)
                    else
                        Cell(NonOccupied, i, j))

            CellGrid(data, areaLength, areaWidth)

        makeCellGrid this.Length this.Width

    member private this.SetupRandomIntGenerator =

        let generateRandomIntNumber seed =
            let generator = Random(seed)

            fun lowerBound upperBound -> generator.Next(lowerBound, upperBound)

        generateRandomIntNumber this.FloorNumber

    member this.GenerateInterior maximumAmountOfObjects placementFunction =
        this.SetupRandomIntGenerator
        |> generateInterior this.GenerateCellGrid this.AreaDataTable maximumAmountOfObjects placementFunction
