namespace RoomInteriorGenerator.Tests

open Helper.Room

module ExpectoTemplate =

    open Expecto

    [<EntryPoint>]
    let main argv =
        Tests.runTestsInAssembly defaultConfig argv
