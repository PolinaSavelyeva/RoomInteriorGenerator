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
    val mutable MaximumAmount: MaximumAmount
    val Variants: DynamicLengthArray<ObjectVariant<'Value>>
    val LengthOfVariantsArray: int
    val PlacementRule: Rule
    val LeafsTable: Option<DynamicLengthArray<DataTableRow<'Value>>>

    new(name, maximumAmount, instancesDynamicArray, placementRule, leafsArray) =
        { Name = name
          MaximumAmount = maximumAmount
          Variants = instancesDynamicArray
          LengthOfVariantsArray = instancesDynamicArray.Length
          PlacementRule = placementRule
          LeafsTable = leafsArray }

    new(name, maximumAmount, instancesArray, placementRule, leafsArray: Option<array<DataTableRow<'Value>>>) =
        { Name = name
          MaximumAmount = maximumAmount
          Variants = DynamicLengthArray instancesArray
          LengthOfVariantsArray = instancesArray.Length
          PlacementRule = placementRule
          LeafsTable =
            if leafsArray.IsNone then
                Option.None
            else
                Some(DynamicLengthArray leafsArray.Value) }

type DataTable<'Value> =
    val Rows: DynamicLengthArray<DataTableRow<'Value>>
    val mutable Length: int

    new(rowsDynamicLengthArray: DynamicLengthArray<DataTableRow<'Value>>) =
        { Rows = rowsDynamicLengthArray
          Length = rowsDynamicLengthArray.Length }

    new(rowsArray: array<DataTableRow<'Value>>) =
        { Rows = DynamicLengthArray rowsArray
          Length = rowsArray.Length }

    member this.Item
        with get i =
            if i >= this.Length || i < 0 then
                failwith "Index out of the range"
            else
                this.Rows[i]

    member this.Delete(index: int) =
        if index < 0 || index >= this.Length then
            failwith "Index out of the range"
        else
            this.Rows.Data[index] <- this.Rows[this.Length - 1]
            this.Length <- this.Length - 1

    member this.IsEmpty = this.Length = 0

    member this.ReduceMaximumAmount (objectRow: DataTableRow<'Value>) dataTableObjectIndex =
        match objectRow.MaximumAmount with
        | Infinity -> ()
        | Finite n ->
            objectRow.MaximumAmount <- Finite(n - 1)

            if objectRow.MaximumAmount = Finite 0 then
                this.Delete dataTableObjectIndex
            else
                ()
