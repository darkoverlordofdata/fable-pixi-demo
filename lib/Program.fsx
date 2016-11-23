
#r "../node_modules/fable-core/Fable.Core.dll"
#r "web/src/MyLibrary/MyLibrary.dll"
#load "../node_modules/fable-import-pixi/Fable.Import.Pixi.fs"

open Fable.Core
open Fable.Import
open Fable.Import.Browser
open Fable.Core.JsInterop
(** Abstrct Game *)
[<AbstractClass>]
type Game(width, height, images) as this =
    let renderer = PIXI.WebGLRenderer(width, height)
    do document.body.appendChild(renderer.view) |> ignore
        
    let rec animate gameTime =
        window.requestAnimationFrame(FrameRequestCallback animate) |> ignore
        this.Draw(gameTime)
        renderer.render(this.Stage)

    member val Stage = PIXI.Container()
    [<DefaultValue>]val mutable Content:obj
    //abstract member Initialize: unit -> unit
    member this.Initialize() =
        this.LoadContent()
    abstract member LoadContent: unit -> unit
    abstract member Update: float -> unit
    abstract member Draw: float -> unit
    member this.Run() =
        for (a, b) in images do PIXI.Globals.loader?add(a, b) |> ignore    
    
        PIXI.Globals.loader.load(System.Func<_,_,_>(fun loader resources ->
            this.Content <- resources
            this.Initialize()
            animate 0. |> ignore
        ))

(** ShmupWarz *)
type ShmupWarz() =
    inherit Game(400., 400., [("bunny", "bunny.png")])
    let mutable bunny = Unchecked.defaultof<PIXI.Sprite>
    
    member this.Initialize() =
        base.Initialize()
    override this.LoadContent() =
        bunny <- PIXI.Sprite(unbox this.Content?bunny?texture)

        // Setup the position and scale of the bunny
        bunny.position.x <- 400.
        bunny.position.y <- 300.

        bunny.scale.x <- 2.
        bunny.scale.y <- 2.

        // Add the bunny to the scene we are building.
        //this.Content?AddSprite(bunny) |> ignore
        this.Stage.addChild(bunny) |> ignore

    override this.Update(gameTime) =
        ()
        
    override this.Draw(gameTime) =        
        // each frame we spin the bunny around a bit
        bunny.rotation <- bunny.rotation + 0.01
        ()


let g = MyLibrary.Class1()
printf "I Found %s" g.X

let game = ShmupWarz()
game.Run()


