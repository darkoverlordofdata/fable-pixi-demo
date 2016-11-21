module ShmupWarzGame

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Input.Touch
open System.Collections.Generic


type ShmupWarz (height, width0, mobile) as this =
    inherit EcsGame(height, width0, mobile)

    let pixelFactor = (if mobile then 2.0f else 1.0f)
    let width = (int)((float32)width0/pixelFactor)
    let mutable Entities = lazy(CreateEntityDB(this.Content, width, height))

    (** Initialize MonoGame *)
    override this.Initialize() =
        this.IsMouseVisible <- true
        base.Initialize()

    (** Load Resources *)
    override this.LoadContent() =
        Entities.Force() |> ignore
        

    (** Game Logic Loop *)
    override this.Update (gameTime) =
        if GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed then 
            this.Exit()

        let delta = float32 gameTime.ElapsedGameTime.TotalSeconds
        let current = Entities.Value

        EnemySpawningSystem(delta, this) |> ignore
        Entities <-  // Everything happens here:
            lazy (current
                 |> List.map(InputSystem(Keyboard.GetState(), Mouse.GetState(), TouchPanel.GetState(), delta, mobile, this))
                 |> List.map(EntitySystem(this, width, height))
                 |> List.map(MovementSystem(delta))
                 |> List.map(ExpiringSystem(delta))
                 |> List.map(ScaleAnimationSystem(delta, this))
                 |> List.map(RemoveOffscreenShipsSystem(this, width, height))
                 |> CollisionSystem(this)
                 )

        // pick up the list when we draw
        this.EntityList <- ActiveEntities (Entities.Force())

    (** Deactivate an Entity *)
    override this.RemoveEntity(id:int) =
        this.Deactivate <- id :: this.Deactivate

    (** Que a Bullet *)
    override this.AddBullet(x: float32, y:float32) =
        this.Bullets <- BulletQue(x, y) :: this.Bullets

    (** Que a Enemy *)
    override this.AddEnemy(enemy : Enemies) =
        match enemy with 
        | Enemies.Enemy1 -> this.Enemies1 <- EnemyQue(enemy) :: this.Enemies1
        | Enemies.Enemy2 -> this.Enemies2 <- EnemyQue(enemy) :: this.Enemies2
        | Enemies.Enemy3 -> this.Enemies3 <- EnemyQue(enemy) :: this.Enemies3
        | _ -> ()

    (** Que an Explosion *)
    override this.AddExplosion(x: float32, y:float32, scale : float32) =
        this.Explosions <- ExplosionQue(x, y, scale) :: this.Explosions

    (** Que an Bang *)
    override this.AddBang(x: float32, y:float32, scale : float32) =
        this.Bangs <- ExplosionQue(x, y, scale) :: this.Bangs