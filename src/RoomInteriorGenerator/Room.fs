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
    val DataTable: DataTable<'Value>

    new(length, width, floorNumber, dataTable) =
        { Length = length
          Width = width
          FloorNumber = floorNumber
          DataTable = dataTable }

    member private this.GenerateCellGrid =
        let roomLength = this.Length
        let roomWidth = this.Width

        let data =
            Array.init (roomLength * roomWidth) (fun index ->
                let i = index / roomLength
                let j = index % roomLength

                if i = 0 || i = roomWidth - 1 || j = 0 || j = roomLength - 1 then
                    AgainstTheWall
                else
                    NonOccupied)

        CellGrid(data, roomLength, roomWidth)

    member private this.SetupRandomIntGenerator =

        let generator = Random(this.FloorNumber)

        fun lowerBound upperBound -> generator.Next(lowerBound, upperBound)

    member this.GenerateInterior maximumAmountOfObjects placementFunction =
        this.SetupRandomIntGenerator
        |> generateInterior this.GenerateCellGrid this.DataTable maximumAmountOfObjects placementFunction
