open System
open ShmupWarzGame
open FSharp.Data
open System.Diagnostics

[<EntryPoint>]
let main argv = 
//    use game = new ShmupWarz(600, 400, false)

//    let value = JsonValue.Load("Content/project.dt")
//    Debug.Print(value.ToString())
    let height = 800
    let width = float32 height / 1.5f
    use game = new ShmupWarz(int height, int width, false)
    game.Run()
    0



