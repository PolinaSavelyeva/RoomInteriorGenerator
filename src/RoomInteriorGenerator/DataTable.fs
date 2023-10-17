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

type AppearancesNumber =
    | EqualTo of int
    | GreaterThanOrEqualTo of int

type DataTableRow =
    val Name: ObjectName
    val Instances: list<obj>
    val Rules: Rules
    val AppearancesNumber: AppearancesNumber
    val OccurrenceProbability: float

type DataTable =
    val Rows: array<DataTableRow>
    member this.GetRules number = 0
    member this.Instances number = 0
    member this.AppearancesNumber number = 0
    member this.OccurrenceProbability number = 0


