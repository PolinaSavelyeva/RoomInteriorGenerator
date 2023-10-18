module RoomInteriorGenerator.DataTable

type ObjectName =
    | Chair
    | Table

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

type Rules =
    | Leaf
    | Node of list<ObjectName * LeafPlacementRule> * NodePlacementRule

type ObjectInstance =
    val Instance: obj
    val ColliderWidth: int
    val ColliderLength: int

    new(instance, colliderWidth, colliderLength) =
        { Instance = instance
          ColliderWidth = colliderWidth
          ColliderLength = colliderLength }

type DataTableRow =
    val Name: ObjectName
    val Instances: list<ObjectInstance>
    val Rules: Rules

type DataTable =
    val Rows: array<DataTableRow>

    member this.Item
        with get i =
            if i >= this.Length then
                failwith "Index out of the range"
            else
                this.Rows[i]

    member this.Length = this.Rows.Length
