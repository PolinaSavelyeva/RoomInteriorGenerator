module RoomInteriorGenerator.DataTable

type LeafPlacementRule =
    | LeftTo
    | RightTo
    | Above
    | Below
    | Behind
    | InFrontOf
    | None

type NodePlacementRule =
    | AgainstTheWall
    | None

type Rules = Node of NodePlacementRule

type ObjectVariant<'Value> =
    val Instance: 'Value
    val freeCellsOnTheRight: int
    val freeCellsOnTheLeft: int
    val freeCellsOnTheTop: int
    val freeCellsOnTheBottom: int

    new(instance, left, right, top, bottom) =
        { Instance = instance
          freeCellsOnTheRight = left
          freeCellsOnTheLeft = right
          freeCellsOnTheTop = top
          freeCellsOnTheBottom = bottom }

type DataTableRow<'Value> =
    val Name: string
    val Instances: array<ObjectVariant<'Value>>
    val LengthOfInstancesArray: int
    val Rules: Rules

    new(name, instancesArray, rules) =
        { Name = name
          Instances = instancesArray
          LengthOfInstancesArray = instancesArray.Length
          Rules = rules }

type DataTable<'Value> =
    val Rows: array<DataTableRow<'Value>>

    new(rowsArray) = { Rows = rowsArray }

    member this.Item
        with get i =
            if i >= this.Length || i < 0 then
                failwith "Index out of the range"
            else
                this.Rows[i]

    member this.Length = this.Rows.Length
