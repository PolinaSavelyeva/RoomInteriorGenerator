module RoomInteriorGenerator.Helper

type LimitedLengthArray<'Value> =
    val mutable Length: int
    val Data: array<'Value>

    new(data: array<'Value>) = { Length = data.Length; Data = data }

    member this.Delete(index: int) =
        this.Data[index] <- this.Data[this.Length + 1]
        this.Length <- this.Length - 1

    member this.Item
        with get i =

            if i >= this.Length || i < 0 then
                failwith "Index out of the range"
            else
                this.Data[i]

    member this.IsEmpty = this.Length = 0
