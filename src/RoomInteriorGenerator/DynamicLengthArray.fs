namespace RoomInteriorGenerator

/// <summary>
/// Represents an array with dynamic length.
/// </summary>
type DynamicLengthArray<'Value> =
    val mutable Length: int
    val Data: array<'Value>

    new(data: array<'Value>) = { Length = data.Length; Data = data }

    /// <summary>
    /// Gets the item at the specified index.
    /// </summary>
    member this.Item
        with get i =
            if i >= this.Length || i < 0 then
                failwith "Index out of the range"
            else
                this.Data[i]

    /// <summary>
    /// Deletes the item at the specified index and reducing the array length.
    /// </summary>
    member this.Delete(index: int) =
        if index < 0 || index >= this.Length then
            failwith "Index out of the range"
        else
            let tmp = this.Data[index]
            this.Data[index] <- this.Data[this.Length - 1]
            this.Data[this.Length - 1] <- tmp

            this.Length <- this.Length - 1

    /// <summary>
    /// Restores the array length to its original length.
    /// </summary>
    member this.Restore = this.Length <- this.Data.Length - 1

    /// <summary>
    /// Checks if the array is empty.
    /// </summary>
    member this.IsEmpty = this.Length = 0
