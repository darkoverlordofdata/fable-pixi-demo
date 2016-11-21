[<AutoOpen>]
module CoreModule
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content

(** IEntity Interface *)
type IEntity = 
    abstract member Id : int with get
    abstract member Name: string with get
    abstract member Active: bool with get
    abstract member EntityType: int with get
    abstract member Layer: int with get
    abstract member Position: Vector2 with get
    abstract member Sprite: Texture2D option with get
    abstract member Scale: Vector2 option with get
    abstract member Tint: Color option with get

(** Base Game Class *)
type AbstractGame (height, width, mobile) as this =
    inherit Game()

    let pixelFactor = if mobile then 2.0f else 1.0f
    let spriteBatch = lazy(new SpriteBatch(this.GraphicsDevice))
    let graphics = new GraphicsDeviceManager(this)
    let mutable fpsRect = Rectangle(0, 0, 16, 24)
    let fntImage = lazy(this.Content.Load<Texture2D>("images/tom-thumb-white"))
    let bgdImage = lazy(this.Content.Load<Texture2D>("images/BackdropBlackLittleSparkBlack"))
    let bgdRect = Rectangle(0, 0, width, height)
    let scaleX = (float32) (width / 320) // pixelFactor
    let scaleY = (float32) (height / 480) // pixelFactor
    let matrix = Matrix.CreateScale(scaleX, scaleY, 1.0f)
    do
        this.Content.RootDirectory <- "Content"
        graphics.IsFullScreen <- mobile
        graphics.PreferredBackBufferWidth <- width
        graphics.PreferredBackBufferHeight <- height
        graphics.ApplyChanges()

    (** Draw the sprite for an Entity *)
    let DrawSprite(spriteBatch:SpriteBatch) (entity:IEntity) =
        match entity.Sprite with
        | Some sprite ->
            let scale =
                match entity.Scale with
                | Some(scale) -> scale
                | None -> Vector2(1.f, 1.f)
            let color = 
                match entity.Tint with 
                | Some(color) -> color
                | None -> Color.White
            let w = int(float32 sprite.Width * scale.X)
            let h = int(float32 sprite.Height * scale.Y)
            let x = int(entity.Position.X) - w/2
            let y = int(entity.Position.Y) - h/2
            spriteBatch.Draw(sprite, Rectangle(x, y, w, h), color)    
        | None -> ()

    (** Draw a FPS in top left corner *)
    let DrawFps(spriteBatch:SpriteBatch, fps:float32)  =
        let ms = int fps
        let d0 = ms / 10        // 9x.xx
        let d1 = ms - d0*10     // x9.xx
        let fp = int((fps - float32 ms) * 100.f)
        let d2 = fp / 10        // xx.9x
        let d3 = fp - d2*10     // xx.x9

        fpsRect.Y <- 24
        fpsRect.X <- 16*(16+d0)
        spriteBatch.Draw(fntImage.Value, Vector2(0.f, 0.f), System.Nullable(fpsRect), Color.White)    
        fpsRect.X <- 16*(16+d1)
        spriteBatch.Draw(fntImage.Value, Vector2(16.f, 0.f), System.Nullable(fpsRect), Color.White)    
        fpsRect.X <- 224
        spriteBatch.Draw(fntImage.Value, Vector2(32.f, 0.f), System.Nullable(fpsRect), Color.White)    
        fpsRect.X <- 16*(16+d2)
        spriteBatch.Draw(fntImage.Value, Vector2(48.f, 0.f), System.Nullable(fpsRect), Color.White)    
        fpsRect.X <- 16*(16+d3)
        spriteBatch.Draw(fntImage.Value, Vector2(64.f, 0.f), System.Nullable(fpsRect), Color.White)    

    member val EntityList:IEntity list = [] with get, set

    (** Game Graphic Loop *)
    override this.Draw(gameTime) =
        this.GraphicsDevice.Clear Color.Black
        spriteBatch.Force().Begin(transformMatrix = Nullable matrix)
        spriteBatch.Force().Draw(bgdImage.Value, bgdRect, Color.White)   
        DrawFps(spriteBatch.Force(), 1.f / float32 gameTime.ElapsedGameTime.TotalSeconds)
        this.EntityList
        |> List.sortBy(fun e -> e.Layer) 
        |> List.iter(DrawSprite(spriteBatch.Force()))
        spriteBatch.Force().End()

