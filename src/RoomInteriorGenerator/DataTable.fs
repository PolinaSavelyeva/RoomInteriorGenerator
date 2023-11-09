module RoomInteriorGenerator.DataTable

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
    val Variants: DynamicLengthArray<ObjectVariant<'Value>>
    val PlacementRule: Rule
    val LeafsTable: Option<DynamicLengthArray<DataTableRow<'Value>>>

    new(name, instancesDynamicArray, placementRule, leafsArray) =
        { Name = name
          Variants = instancesDynamicArray
          PlacementRule = placementRule
          LeafsTable = leafsArray }

    new(name, instancesArray, placementRule, leafsArray: Option<array<DataTableRow<'Value>>>) =
        { Name = name
          Variants = DynamicLengthArray instancesArray
          PlacementRule = placementRule
          LeafsTable =
            if leafsArray.IsNone then
                Option.None
            else
                Some(DynamicLengthArray leafsArray.Value) }

    member this.RestoreVariants = this.Variants.Restore

    member this.RestoreLeafsTable =
        if this.LeafsTable.IsSome then
            this.LeafsTable.Value.Restore

    member this.LengthOfVariantsArray = this.Variants.Length

type DataTable<'Value> =
    val Rows: DynamicLengthArray<DataTableRow<'Value>>

    new(rowsDynamicLengthArray: DynamicLengthArray<DataTableRow<'Value>>) = { Rows = rowsDynamicLengthArray }

    new(rowsArray: array<DataTableRow<'Value>>) = { Rows = DynamicLengthArray rowsArray }

    member this.Length = this.Rows.Length

    member this.Item
        with get i = this.Rows[i]

    member this.Delete(index: int) = this.Rows.Delete index

    member this.Restore =
        this.Rows.Restore

        Array.iter
            (fun (n: DataTableRow<'Value>) ->
                n.RestoreVariants
                n.RestoreLeafsTable)
            this.Rows.Data

    member this.IsEmpty = this.Rows.IsEmpty
