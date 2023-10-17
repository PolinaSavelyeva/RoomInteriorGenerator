module RoomInteriorGenerator.Area

let divideAreaToCells areaLength areaWidth = 0

type Area =
    // Room's lower left corner must be at (0,0)
    val Length: int
    val Width: int

    member this.divideToCells =
        divideAreaToCells this.Length this.Width
    member this.generateInterior  = this.divideToCells |>
