module RoomInteriorGenerator.Area

open InteriorGenerateAlgorithm
open DataTable
open System
open Cell

type Area<'Value> =
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

    member this.MakeCellGrid =
        let _makeCellGrid areaLength areaWidth =

            let data =
                Array.init (areaLength * areaWidth) (fun index ->
                    let i = index / areaLength
                    let j = index % areaLength

                    if i = 0 || i = areaWidth - 1 || j = 0 || j = areaLength - 1 then
                        Cell(AgainstTheWall, i, j)
                    else
                        Cell(NonOccupied, i, j))

            CellGrid(data, areaLength, areaWidth)

        _makeCellGrid this.Length this.Width

    member this.InitializeIntGenerator =

        let _generateRandomIntNumber seed =
            let generator = Random(seed)

            fun lowerBound upperBound -> generator.Next(lowerBound, upperBound)

        _generateRandomIntNumber this.FloorNumber

    member this.GenerateInterior maximumAmountOfObjects placementFunction =
        this.InitializeIntGenerator
        |> generateInterior this.MakeCellGrid this.AreaDataTable maximumAmountOfObjects placementFunction
