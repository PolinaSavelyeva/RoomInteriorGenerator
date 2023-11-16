namespace RoomInteriorGenerator

open DataTable
open Cell
open PCG
open System

/// <summary>
/// Represents a room, defined by a DataTable containing objects and their variants.
/// </summary>
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

    /// <summary>
    /// Generates a CellGrid based on the room dimensions, marking perimeter cells as AgainstTheWall and leaving interior cells as NonOccupied.
    /// </summary>
    member private this.GenerateCellGrid =
        let roomLength = this.Length
        let roomWidth = this.Width

        let data =
            Array.init (roomLength * roomWidth) (fun index ->
                let i = index / roomLength
                let j = index % roomLength

                if j = 0 && i <> roomWidth - 1 && i <> 0 then
                    AgainstTheLeftWall
                elif i = 0 && j <> 0 && j <> roomLength - 1 then
                    AgainstTheTopWall
                elif j = roomLength - 1 && i <> 0 && i <> roomWidth - 1 then
                    AgainstTheLeftWall
                elif i = roomWidth - 1 && j <> 0 && j <> roomLength - 1 then
                    AgainstTheLeftWall
                elif
                    (i = 0 && j = 0)
                    || (i = 0 && j = roomLength - 1)
                    || (i = roomWidth - 1 && j = roomLength - 1)
                    || (i = roomWidth - 1 && j = 0)
                then
                    Corner
                else
                    NonOccupied)

        CellGrid(data, roomLength, roomWidth)

    /// <summary>
    /// Sets up a random integer generator with a seed based on the room's floor number.
    /// </summary>
    /// <returns>A function that generates random integers within specified bounds</returns>
    member private this.SetupRandomIntGenerator =

        let generator = Random(this.FloorNumber)

        fun lowerBound upperBound -> generator.Next(lowerBound, upperBound)

    /// <summary>
    /// Generates the interior layout of the room by placing objects on the CellGrid using a DataTable and a specified placement function.
    /// </summary>
    /// <param name="maximumAmountOfObjects">The maximum number of objects to be placed</param>
    /// <param name="placementFunction">The function responsible for placing objects on the room</param>
    member this.GenerateInterior maximumAmountOfObjects placementFunction =
        this.SetupRandomIntGenerator
        |> generateInterior this.GenerateCellGrid this.DataTable maximumAmountOfObjects placementFunction
