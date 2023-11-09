module RoomInteriorGenerator.DataTable

open Helper

type LeafPlacementRule =
    | LeftTo
    | RightTo
    | Behind
    | InFrontOf
    | Anywhere

type NodePlacementRule =
    | AgainstTheWall
    | None

type Rule =
    | Node of NodePlacementRule
    | Leaf of LeafPlacementRule

type ObjectVariant<'Value> =
    val Instance: 'Value
    val FreeCellsOnTheRight: int
    val FreeCellsOnTheLeft: int
    val FreeCellsOnTheTop: int
    val FreeCellsOnTheBottom: int

    new(instance, left, right, top, bottom) =
        { Instance = instance
          FreeCellsOnTheRight = left
          FreeCellsOnTheLeft = right
          FreeCellsOnTheTop = top
          FreeCellsOnTheBottom = bottom }

type MaximumAmount =
    | Infinity
    | Finite of int

type DataTableRow<'Value> =
    val Name: string
    val mutable MaximumAmount: MaximumAmount
    val Variants: DynamicLengthArray<ObjectVariant<'Value>>
    val PlacementRule: Rule
    val LeafsTable: Option<DynamicLengthArray<DataTableRow<'Value>>>

    new(name, maximumAmount, instancesDynamicArray, placementRule, leafsArray) =
        { Name = name
          MaximumAmount = maximumAmount
          Variants = instancesDynamicArray
          PlacementRule = placementRule
          LeafsTable = leafsArray }

    new(name, maximumAmount, instancesArray, placementRule, leafsArray: Option<array<DataTableRow<'Value>>>) =
        { Name = name
          MaximumAmount = maximumAmount
          Variants = DynamicLengthArray instancesArray
          PlacementRule = placementRule
          LeafsTable =
            if leafsArray.IsNone then
                Option.None
            else
                Some(DynamicLengthArray leafsArray.Value) }

    member this.LengthOfVariantsArray = this.Variants.Length

type DataTable<'Value> =
    val Rows: DynamicLengthArray<DataTableRow<'Value>>

    new(rowsDynamicLengthArray: DynamicLengthArray<DataTableRow<'Value>>) = { Rows = rowsDynamicLengthArray }

    new(rowsArray: array<DataTableRow<'Value>>) = { Rows = DynamicLengthArray rowsArray }

    member this.Length = this.Rows.Length

    member this.Item
        with get i = this.Rows[i]

    member this.Delete(index: int) = this.Rows.Delete index

    member this.IsEmpty = this.Rows.IsEmpty

    member this.ReduceMaximumAmount (objectRow: DataTableRow<'Value>) dataTableObjectIndex =
        match objectRow.MaximumAmount with
        | Infinity -> ()
        | Finite n ->
            objectRow.MaximumAmount <- Finite(n - 1)

            if objectRow.MaximumAmount = Finite 0 then
                this.Delete dataTableObjectIndex
            else
                ()
