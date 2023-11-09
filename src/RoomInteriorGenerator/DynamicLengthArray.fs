namespace RoomInteriorGenerator

type DynamicLengthArray<'Value> =
    val mutable Length: int
    val Data: array<'Value>

    new(data: array<'Value>) = { Length = data.Length; Data = data }

    member this.Item
        with get i =
            if i >= this.Length || i < 0 then
                failwith "Index out of the range"
            else
                this.Data[i]

    member this.Delete(index: int) =
        if index < 0 || index >= this.Length then
            failwith "Index out of the range"
        else
            let tmp = this.Data[index]
            this.Data[index] <- this.Data[this.Length - 1]
            this.Data[this.Length - 1] <- tmp

            this.Length <- this.Length - 1

    member this.Restore = this.Length <- this.Data.Length - 1

    member this.IsEmpty = this.Length = 0
