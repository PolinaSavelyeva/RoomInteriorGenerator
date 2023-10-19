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

type ObjectInstance<'Value> =
    val Instance: 'Value
    val ColliderLength: int
    val ColliderWidth: int

    new(instance, colliderLength, colliderWidth) =
        { Instance = instance
          ColliderLength = colliderLength
          ColliderWidth = colliderWidth }

type DataTableRow<'Value> =
    val Name: string
    val Instances: list<ObjectInstance<'Value>>
    val Rules: Rules

    new(name, instancesList, rules) =
        { Name = name
          Instances = instancesList
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
