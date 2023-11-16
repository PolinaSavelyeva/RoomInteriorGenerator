module RoomInteriorGenerator.DataTable

/// <summary>
/// Enumerates child placement rules for objects to be placed in the room.
/// </summary>
type LeafPlacementRule =
    | LeftTo
    | RightTo
    | Behind
    | InFrontOf
    | Anywhere

/// <summary>
/// Enumerates node placement rules for objects to be placed in the room.
/// </summary>
type NodePlacementRule =
    | AgainstTheLeftWall
    | AgainstTheRightWall
    | AgainstTheTopWall
    | AgainstTheBottomWall
    | InTheCorner
    | None

/// <summary>
/// Represents placement rules for objects, which can be either node or leaf rules.
/// </summary>
type Rule =
    | Node of NodePlacementRule
    | Leaf of LeafPlacementRule

/// <summary>
/// Represents a variant of an object with specific instance data and dimensions.
/// </summary>
type ObjectVariant<'Value> =
    val Variant: 'Value
    val FreeCellsOnTheRight: int
    val FreeCellsOnTheLeft: int
    val FreeCellsOnTheTop: int
    val FreeCellsOnTheBottom: int

    new(instance, left, right, top, bottom) =
        { Variant = instance
          FreeCellsOnTheRight = left
          FreeCellsOnTheLeft = right
          FreeCellsOnTheTop = top
          FreeCellsOnTheBottom = bottom }

/// <summary>
/// Represents a row in a DataTable containing object variants, placement rules, and optional leaf tables.
/// </summary>
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

    new(name, instancesArray, placementRule, leafsArray: array<DataTableRow<'Value>>) =
        { Name = name
          Variants = DynamicLengthArray instancesArray
          PlacementRule = placementRule
          LeafsTable =
            if Array.isEmpty leafsArray then
                Option.None
            else
                Some(DynamicLengthArray leafsArray) }

    /// <summary>
    /// Restores the variants array to its original length.
    /// </summary>
    member this.RestoreVariants = this.Variants.Restore

    /// <summary>
    /// Restores the leafs table array to its original length if it is not Option.None.
    /// </summary>
    member this.RestoreLeafsTable =
        if this.LeafsTable.IsSome then
            this.LeafsTable.Value.Restore

    /// <summary>
    /// Gets the length of the variants array.
    /// </summary>
    member this.LengthOfVariantsArray = this.Variants.Length

/// <summary>
/// Represents a DataTable containing rows of DataTableRow.
/// </summary>
type DataTable<'Value> =
    val Rows: DynamicLengthArray<DataTableRow<'Value>>

    new(rowsDynamicLengthArray: DynamicLengthArray<DataTableRow<'Value>>) = { Rows = rowsDynamicLengthArray }

    new(rowsArray: array<DataTableRow<'Value>>) = { Rows = DynamicLengthArray rowsArray }

    /// <summary>
    /// Gets the length of the DataTable.
    /// </summary>
    member this.Length = this.Rows.Length

    /// <summary>
    /// Gets the DataTableRow at the specified index.
    /// </summary>
    member this.Item
        with get i = this.Rows[i]

    /// <summary>
    /// Deletes the DataTableRow at the specified index.
    /// </summary>
    member this.Delete(index: int) = this.Rows.Delete index

    /// <summary>
    /// Restores the DataTable and its rows to their original lengths.
    /// </summary>
    member this.Restore =
        this.Rows.Restore

        Array.iter
            (fun (n: DataTableRow<'Value>) ->
                n.RestoreVariants
                n.RestoreLeafsTable)
            this.Rows.Data

    /// <summary>
    /// Checks if the DataTable is empty.
    /// </summary>
    member this.IsEmpty = this.Rows.IsEmpty
