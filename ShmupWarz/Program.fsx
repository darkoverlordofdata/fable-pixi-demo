(** ShmupWarz Game Demo *)

#r "../node_modules/fable-core/Fable.Core.dll"
#load "../node_modules/fable-import-pixi/Fable.Import.Pixi.fs"

#load "./Input/Mouse.fsx"
#load "./Input/Keyboard.fsx"

open Fable.Core
open Fable.Import
open Fable.Import.Browser
open Fable.Core.JsInterop
open Mouse
open Keyboard

type Color = 
    | White = 0xffffff
    | AliceBlue = 0xf0f8ff
    | AntiqueWhite = 0xfaebd7
    | Aqua = 0x00ffff
    | Aquamarine = 0x7fffd4
    | Azure = 0xf0ffff
    | Beige = 0xf5f5dc
    | Bisque = 0xffe4c4
    | Black = 0x000000
    | BlanchedAlmond = 0xffebcd
    | Blue = 0x0000ff
    | BlueViolet = 0x8a2be2
    | Brown = 0xa52a2a
    | BurlyWood = 0xdeb887
    | CadetBlue = 0x5f9ea0
    | Chartreuse = 0x7fff00
    | Chocolate = 0xd2691e
    | Coral = 0xff7f50
    | CornflowerBlue = 0x6495ed
    | Cornsilk = 0xfff8dc
    | Crimson = 0xdc143c
    | Cyan = 0x00ffff
    
    | GreenYellow = 0xadff2f
    | LightGoldenrodYellow = 0xfafad2
    | PaleGoldenrod = 0xeee8aa

 
(** src:Core.fs *)

(** Abstrct Game *)
[<AbstractClass>]
type Game(width, height, images) as this =
    let mutable previousTime = 0.0
    let mutable elapsedTime = 0.0
    let mutable totalFrames = 0
    let renderer = PIXI.WebGLRenderer(width, height)
    do document.body.appendChild(renderer.view) |> ignore        
    let rec animate timeStamp =
        let t = if previousTime>0.0 then previousTime else timeStamp
        previousTime <- timeStamp
        let delta = (timeStamp - t) * 0.001
        totalFrames <- totalFrames + 1
        elapsedTime <- elapsedTime + delta
        if elapsedTime > 1.0 then
            this.fps <- totalFrames
            totalFrames <- 0
            elapsedTime <- 0.0

        window.requestAnimationFrame(FrameRequestCallback animate) |> ignore
        this.Update(delta)
        this.Draw(delta)
        renderer.render(this.spriteBatch)
    member val spriteBatch = PIXI.Container()
    member val fps = 0 with get,set
    [<DefaultValue>]val mutable Content:obj
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

(** src:Components.fs *)

(** Layer - All entities need a display layer *)
type Layer =
    | DEFAULT           = 0
    | BACKGROUND        = 1
    | TEXT              = 2
    | LIVES             = 3
    | ENEMY1            = 4
    | ENEMY2            = 5
    | ENEMY3            = 6
    | PLAYER            = 7
    | BULLET            = 8
    | EXPLOSION         = 9
    | BANG              = 10
    | PARTICLE          = 11
    | HUD               = 12

(** EntityType Component *)
type EntityType =
    | Background        = 0
    | Bullet            = 1
    | Enemy             = 2
    | Explosion         = 3
    | Particle          = 4
    | Player            = 5

(** Sound Effect Component *)
type Effect =
    | PEW               = 0
    | ASPLODE           = 1
    | SMALLASPLODE      = 2

(** Enemy Type Component *)
type Enemies =
    | Enemy1            = 0
    | Enemy2            = 1
    | Enemy3            = 2

(** Health Component *)
type Health =
    {
        CurHealth: int;
        MaxHealth: int;
    }

(** Create a Health Component *)
let CreateHealth(curHealth: int, maxHealth : int) =
    {
        CurHealth = curHealth;
        MaxHealth = maxHealth;
    }

(** Tween Component *)
type Tween =
    {
        Min : float;
        Max : float;
        Speed : float;
        Repeat : bool;
        Active : bool;
    }
(** Create a Scale Animation Component *)
let CreateTween(min: float, max: float, speed: float, repeat: bool, active: bool) =
    {
        Min = min;
        Max = max;
        Speed = speed;
        Repeat = repeat;
        Active = active;
    } 



(** Request an enemy *)
type EnemyQueItem =
    {
        Enemy : Enemies;
    }
let EnemyQue(enemy : Enemies) : EnemyQueItem =
    {
        Enemy = enemy;
    }


(** Request an explosion *)
type ExplosionQueItem =
    {
        X: float;
        Y: float;
        Scale : float;
    }
let ExplosionQue(x:float, y:float, scale : float) : ExplosionQueItem =
    {
        X = x;
        Y = y;
        Scale = scale;
    }

(** Request a bullet *)
type BulletQueItem =
    {
        X: float;
        Y: float;
    }
let BulletQue(x:float, y:float) : BulletQueItem =
    {
        X = x;
        Y = y;
    }
    
(** src:Entity.fs *)
(**
 * Entity Factory
 *
 *)

let rnd = System.Random()
let mutable UniqueId = 0
let NextUniqueId() = 
    UniqueId <- UniqueId + 1
    UniqueId

(** Entity is a record of components *)
type Entity = 
    {
        Id              : int;                  (* Unique sequential id *)
        Name            : string;               (* Display name *)
        Active          : bool;                 (* In use *)
        EntityType      : EntityType;           (* Category *)
        Layer           : Layer;                (* Display Layer *)
        Position        : PIXI.Point;           (* Position *)
        Sprite          : PIXI.Sprite option;   (* Sprite *)
        Scale           : PIXI.Point option;    (* Display Scale *)
        Tint            : Color option;         (* Color to use as tint *)
        Bounds          : int option;           (* For Hit Detection *)
        Expires         : float option;         (* Entity duration *)
        Health          : Health option;        (* Points *)
        Tween           : Tween option;         (* Explosion tweens *)
        Size            : PIXI.Point;           (* Sprite size *)
        Velocity        : PIXI.Point option;    (* Movement speed *)
    }

(** Create a Player Entity *)
let CreatePlayer (content:obj, x: float, y: float) =
    let position = PIXI.Point(0., 0.)
    let sprite = PIXI.Sprite(unbox content?fighter?texture)
    sprite.anchor <- PIXI.Point(0.5, 0.5)
    {
        Id = NextUniqueId();
        Name = "Player";
        Active = true;
        EntityType = EntityType.Player; 
        Layer = Layer.PLAYER;
        Position = position; 
        Scale = None;
        Sprite = Some(sprite);
        Tint = None;

        Bounds = Some(43);
        Expires = None;
        Health = Some(CreateHealth(100, 100));
        Velocity = Some(PIXI.Point(0., 0.));
        Tween = None;
        Size = PIXI.Point(float sprite.width, float sprite.height);
    }
     
(** Create a Bullet Entity *)
let CreateBullet (content:obj, x: float, y: float) =
    let position = PIXI.Point(x, y)
    let sprite = PIXI.Sprite(unbox content?bullet?texture)
    sprite.anchor <- PIXI.Point(0.5, 0.5)
    {
        Id = NextUniqueId();
        Name = "Bullet";
        Active = false;
        EntityType = EntityType.Bullet; 
        Layer = Layer.BULLET;
        Position = position; 
        Scale = None;
        Sprite = Some(sprite);
        Tint = Some(Color.GreenYellow);

        Bounds = Some(5);
        Expires = Some(0.1);
        Health = None;
        Velocity = Some(PIXI.Point(0., -800.));
        Tween = None;
        Size = PIXI.Point(float sprite.width, float sprite.height);
    }

(** Create Enemy *)
let CreateEnemy1 (content:obj, width: int, height: int)  =
    let position = PIXI.Point(float(rnd.Next(width)), 100.)
    let sprite = PIXI.Sprite(unbox content?enemy1?texture)
    sprite.anchor <- PIXI.Point(0.5, 0.5)
    {
        Id = NextUniqueId();
        Name = "Enemy1";
        Active = false;
        EntityType = EntityType.Enemy; 
        Layer = Layer.ENEMY1;
        Position = position; 
        Scale = None;
        Sprite = Some(sprite);
        Tint = None;

        Bounds = Some(20);
        Expires = None
        Health = Some(CreateHealth(10, 10));
        Velocity = Some(PIXI.Point(0., 40.));
        Tween = None;
        Size = PIXI.Point(float sprite.width, float sprite.height);
    }
(** Create Enemy *)
let CreateEnemy2 (content:obj, width: int, height: int) =
    let position = PIXI.Point(float(rnd.Next(width)), 200.)
    let sprite = PIXI.Sprite(unbox content?enemy2?texture)
    sprite.anchor <- PIXI.Point(0.5, 0.5)
    {
        Id = NextUniqueId();
        Name = "Enemy2";
        Active = false;
        EntityType = EntityType.Enemy; 
        Layer = Layer.ENEMY2;
        Position = position; 
        Scale = None;
        Sprite = Some(sprite);
        Tint = None;

        Bounds = Some(40);
        Expires = None
        Health = Some(CreateHealth(20, 20));
        Velocity = Some(PIXI.Point(0., 30.));
        Tween = None;
        Size = PIXI.Point(float sprite.width, float sprite.height);
    }

(** Create Enemy *)
let CreateEnemy3 (content:obj, width: int, height: int)  =
    let position = PIXI.Point(float(rnd.Next(width)), 300.)
    let sprite = PIXI.Sprite(unbox content?enemy3?texture)
    sprite.anchor <- PIXI.Point(0.5, 0.5)
    {
        Id = NextUniqueId();
        Name = "Enemy3";
        Active = false;
        EntityType = EntityType.Enemy; 
        Layer = Layer.ENEMY3;
        Position = position; 
        Scale = None;
        Sprite = Some(sprite);
        Tint = None;

        Bounds = Some(70);
        Expires = None
        Health = Some(CreateHealth(60, 60));
        Velocity = Some(PIXI.Point(0., 20.));
        Tween = None;
        Size = PIXI.Point(float sprite.width, float sprite.height);
    }

(** Create Big Explosion *)
let CreateExplosion (content:obj, x: float, y: float, scale:float) =
    let position = PIXI.Point(x, y)
    let sprite = PIXI.Sprite(unbox content?explosion?texture)
    sprite.anchor <- PIXI.Point(0.5, 0.5)
    {
        Id = NextUniqueId();
        Name = "Explosion";
        Active = false;
        EntityType = EntityType.Explosion; 
        Layer = Layer.EXPLOSION;
        Position = position; 
        Scale = Some(PIXI.Point(scale, scale))
        Sprite = Some(sprite);
        Tint = Some(Color.LightGoldenrodYellow);

        Bounds = None;
        Expires = Some(0.5);
        Health = None;
        Velocity = None;
        Tween = Some(CreateTween(scale/100., scale, -3., false, true));
        Size = PIXI.Point(float sprite.width, float sprite.height);
    }

let CreateBang (content:obj, x: float, y: float, scale:float) =
    let position = PIXI.Point(x, y)
    let sprite = PIXI.Sprite(unbox content?bang?texture)
    sprite.anchor <- PIXI.Point(0.5, 0.5)
    {
        Id = NextUniqueId();
        Name = "Bang";
        Active = false;
        EntityType = EntityType.Explosion; 
        Layer = Layer.BANG;
        Position = position; 
        Scale = Some(PIXI.Point(scale, scale))
        Sprite = Some(sprite);
        Tint = Some(Color.PaleGoldenrod);

        Bounds = None;
        Expires = Some(0.5);
        Health = None;
        Velocity = None;
        Tween = Some(CreateTween(scale/100., scale, -3., false, true));
        Size = PIXI.Point(float sprite.width, float sprite.height);
    }

(**
 * Create the Entity DataBase
 *) 
let CreateEntityDB(content, width, height) = 
    [
        CreatePlayer(content, 0., 0.);
        CreateBang(content, 0., 0., 1.);
        CreateBang(content, 0., 0., 1.);
        CreateBang(content, 0., 0., 1.);
        CreateBang(content, 0., 0., 1.);
        CreateBang(content, 0., 0., 1.);
        CreateBang(content, 0., 0., 1.);
        CreateBang(content, 0., 0., 1.);
        CreateBang(content, 0., 0., 1.);
        CreateExplosion(content, 0., 0., 1.);
        CreateExplosion(content, 0., 0., 1.);
        CreateExplosion(content, 0., 0., 1.);
        CreateExplosion(content, 0., 0., 1.);
        CreateExplosion(content, 0., 0., 1.);
        CreateExplosion(content, 0., 0., 1.);
        CreateExplosion(content, 0., 0., 1.);
        CreateExplosion(content, 0., 0., 1.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateBullet(content, 0., 0.);
        CreateEnemy1(content, 0, 0);
        CreateEnemy1(content, 0, 0);
        CreateEnemy1(content, 0, 0);
        CreateEnemy1(content, 0, 0);
        CreateEnemy1(content, 0, 0);
        CreateEnemy1(content, 0, 0);
        CreateEnemy1(content, 0, 0);
        CreateEnemy1(content, 0, 0);
        CreateEnemy1(content, 0, 0);
        CreateEnemy2(content, 0, 0);
        CreateEnemy2(content, 0, 0);
        CreateEnemy2(content, 0, 0);
        CreateEnemy2(content, 0, 0);
        CreateEnemy2(content, 0, 0);
        CreateEnemy2(content, 0, 0);
        CreateEnemy3(content, 0, 0);
        CreateEnemy3(content, 0, 0);
        CreateEnemy3(content, 0, 0);
        CreateEnemy3(content, 0, 0);

    ]


(** src:Entities.fs *)
(**
 * The abstract EscGame provides interface and lists to
 * use for adding and removing entities
 * This allows systems to hold a forward reference to the game object
 *)
[<AbstractClass>]
type EcsGame(height, width, images) =
    inherit Game(height, width, images)
    member val Bullets = List.empty<BulletQueItem> with get,set
    member val Deactivate = List.empty<int> with get,set
    member val Enemies1 = List.empty<EnemyQueItem> with get,set
    member val Enemies2 = List.empty<EnemyQueItem> with get,set
    member val Enemies3 = List.empty<EnemyQueItem> with get,set
    member val Explosions = List.empty<ExplosionQueItem> with get,set
    member val Bangs = List.empty<ExplosionQueItem> with get,set

    abstract member AddBullet : float * float -> unit
    abstract member AddEnemy : Enemies -> unit 
    abstract member AddExplosion : float * float * float -> unit
    abstract member AddBang : float * float * float -> unit
    abstract member RemoveEntity: int -> unit

(**
 * Returns a list of active entities for drawing.
 * No need to rev the returned list, they will be sorted by layer.
 *)
let ActiveEntities (input:Entity list) =
    let rec _activeEntities (input:Entity list) (output:Entity list) =
        match input with
        | x::xs when x.Active -> _activeEntities xs (x::output)
        | _::xs -> _activeEntities xs output 
        | [] -> output
    _activeEntities input []

(** src:Systems/EntitySystem.fs *)
(**
 * Activate / Deactiveate Entities as needed 
 *)
let EntitySystem (game:EcsGame, width: int, height: int) entity =

    match entity.Active with
    | true -> 
        let mutable removed = false
        let rec removeIf l predicate =
            match l with
            | [] -> []
            | x::rest -> 
                if predicate(x) then 
                    removed <- true
                    (removeIf rest predicate) 
                else 
                    x::(removeIf rest predicate)

        let len = game.Deactivate.Length
        game.Deactivate <- (removeIf game.Deactivate (fun id -> id = entity.Id))
        {
            entity with 
                Active = not removed;
        }

    | false -> 
        match entity.Layer with
        | Layer.BULLET ->
            match game.Bullets with
            | [] -> entity
            | bullet :: rest ->
                game.Bullets <- rest
                {            
                    entity with
                        Active = true;
                        Expires = Some(0.5);                        
                        Position = PIXI.Point(bullet.X, bullet.Y);
                }
        | Layer.ENEMY1 ->
            match game.Enemies1 with
            | [] -> entity
            | enemy :: rest ->
               game.Enemies1 <- rest
               {
                    entity with 
                        Active = true;
                        Position = PIXI.Point(float(rnd.Next(width-35)), 91./2.0);
                        Health = Some(CreateHealth(10, 10));
                }
        | Layer.ENEMY2 ->
            match game.Enemies2 with
            | [] -> entity
            | enemy :: rest ->
                game.Enemies2 <- rest
                {
                    entity with 
                        Active = true;
                        Position = PIXI.Point(float(rnd.Next(width-86)), 172./2.);
                        Health = Some(CreateHealth(20, 20));                
                }
        | Layer.ENEMY3 ->
            match game.Enemies3 with
            | [] -> entity
            | enemy :: rest ->
                game.Enemies3 <- rest
                {
                    entity with 
                        Active = true;
                        Position = PIXI.Point(float(rnd.Next(width-160)), 320./2.);
                        Health = Some(CreateHealth(60, 60));                
                }
        | Layer.EXPLOSION ->
            match game.Explosions with
            | [] -> entity
            | exp :: rest ->
                game.Explosions <- rest
                {
                    entity with 
                        Active = true;
                        Expires = Some(0.2);                        
                        Scale = Some(PIXI.Point(exp.Scale, exp.Scale));
                        Position = PIXI.Point(exp.X, exp.Y);
                }
        | Layer.BANG ->
            match game.Bangs with
            | [] -> entity
            | exp :: rest ->
                game.Bangs <- rest
                {
                    entity with 
                        Active = true;
                        Expires = Some(0.2);                        
                        Scale = Some(PIXI.Point(exp.Scale, exp.Scale));
                        Position = PIXI.Point(exp.X, exp.Y);
                }
        | _ -> entity

(** src:Systems/CollisionSystem.fs *)
(** Return Rect defining the current bounds *)
let BoundingRect(entity) =
    let x = entity.Position.x
    let y = entity.Position.y
    let w = entity.Size.x
    let h = entity.Size.y
    PIXI.Rectangle(x - w/2., y - h/2., w, h):>PIXI.HitArea

(** Collision Handler for Entities *)
let CollisionSystem (game:EcsGame) entities =

    let findCollision a b =
        match a.EntityType, a.Active, b.EntityType, b.Active with
        | EntityType.Enemy, true, EntityType.Bullet, true -> 
            game.AddBang(b.Position.x, b.Position.y, 1.0)
            game.RemoveEntity(b.Id)
            match a.Health with
            | Some(h) ->
                let health = h.CurHealth-1
                if health <= 0 then
                    game.AddExplosion(b.Position.x, b.Position.y, 0.5)
                    {
                        a with
                            Active = false;
                    }
                else
                    {
                        a with 
                            Health = Some(CreateHealth(health, h.MaxHealth));
                    }

            | None -> a
        | _ -> a

    let rec figureCollisions (entity:Entity) (sortedEntities:Entity list) =
        match sortedEntities with
        | [] -> entity
        | x :: xs -> 
            let a = if (BoundingRect(entity).contains(x.Position.x, x.Position.y)) then
                        findCollision entity x
                    else
                        entity
            figureCollisions a xs

    let rec fixCollisions (toFix:Entity list) (alreadyFixed:Entity list) =
        match toFix with
        | [] -> alreadyFixed
        | x :: xs -> 
            let a = figureCollisions x alreadyFixed
            fixCollisions xs (a::alreadyFixed)

    fixCollisions entities []

(** src:Systems/EnemySpawningSystem.fs *)
type Timers =
    | Timer1 = 2
    | Timer2 = 7
    | Timer3 = 13

let mutable enemyT1 = float(Timers.Timer1)
let mutable enemyT2 = float(Timers.Timer2)
let mutable enemyT3 = float(Timers.Timer3)

let EnemySpawningSystem (delta:float, game:EcsGame)  =
    let spawnEnemy (t:float, enemy) =
        let delta = t - delta

        if delta < 0.0 then
            game.AddEnemy(enemy)
            match enemy with
            | Enemies.Enemy1 -> float(Timers.Timer1)
            | Enemies.Enemy2 -> float(Timers.Timer2)
            | Enemies.Enemy3 -> float(Timers.Timer3)
            | _ -> 0.0
        else delta

    enemyT1 <- spawnEnemy(enemyT1, Enemies.Enemy1)
    enemyT2 <- spawnEnemy(enemyT2, Enemies.Enemy2)
    enemyT3 <- spawnEnemy(enemyT3, Enemies.Enemy3)
    
(** src:Systems/InputSystem.fs *)
let mutable timeToFire = 0.0
(** Get Player Input *)
let InputSystem (delta:float, mobile:bool, game:EcsGame) entity =
    let pf = if mobile then 2.0 else 1.0
    match entity.EntityType with
    | EntityType.Player -> 

        let position =
            let newPosition = Mouse.position
            if Keyboard.isPressed 90 then
                timeToFire <- timeToFire - delta
                if timeToFire <= 0.0 then
                    game.AddBullet(newPosition.x-27.0, newPosition.y)
                    game.AddBullet(newPosition.x+27.0, newPosition.y)
                    timeToFire <- 0.1
                newPosition
            else
                if Mouse.down then
                    timeToFire <- timeToFire - delta
                    if timeToFire <= 0.0 then
                        game.AddBullet(newPosition.x-27.0, newPosition.y)
                        game.AddBullet(newPosition.x+27.0, newPosition.y)
                        timeToFire <- 0.1
                    newPosition
                else
                    newPosition

        (* Set Player Position *)
        { entity with Position = position;  }
    | _ ->
        entity

(** src:Systems/MovementSystem.fs *)
(** Movement System *)
let MovementSystem (delta:float) entity =

    match entity.Velocity, entity.Active with

    | Some(velocity), true ->
        let x = entity.Position.x + velocity.x * delta
        let y = entity.Position.y + velocity.y * delta
        { entity with Position = PIXI.Point(float x, float y)}

    | _ -> entity


(** src:Systems/ExpiringSSystem.fs *)
(** 
 * Expiring System 
 *
 * Destroy entities when their time is up
 *)
let ExpiringSystem (delta:float) entity =
    match entity.Expires, entity.Active with
    | Some(v), true ->
        let exp = v - delta
        let active = if exp > 0. then true else false
        { 
            entity with 
                Expires = Some(exp);
                Active = active;
        }
    | _ -> entity


(** src:Systems/RemoveOffscreenShipsSystem.fs *)
let RemoveOffscreenShipsSystem (game:EcsGame, width: int, height: int) entity =
    match entity.EntityType, entity.Active with
    | EntityType.Enemy, true when int entity.Position.y > height ->
        { 
            entity with 
                Active = false;
        }
    | _ -> entity

(** src:Systems/TweenSystem.fs *)
let TweenSystem (delta:float, game:EcsGame) entity =
    match (entity.Scale, entity.Tween, entity.Active) with
    | Some(scale), Some(sa), true ->        
        let mutable x = scale.x + (sa.Speed * delta)
        let mutable y =  scale.y + (sa.Speed * delta)
        let mutable active = sa.Active
        if x > sa.Max then
            x <- sa.Max
            y <- sa.Max
            active <- false
        elif x < sa.Min then
            x <- sa.Min
            y <- sa.Min
            active <- false

        {
            entity with
                Scale = Some(PIXI.Point(x, y));
                Tween = Some(CreateTween(sa.Min, sa.Max, sa.Speed, sa.Repeat, active));
        }

    | _ -> 
        entity


(** src:ShmupWarzGame.fs *)
(** ShmupWarz *)
type ShmupWarz(height, width0, mobile) as this =
    inherit EcsGame(height, width0, [
                            ("background", "images/BackdropBlackLittleSparkBlack.png");
                            ("bang", "images/bang.png");
                            ("bullet", "images/bullet.png");
                            ("enemy1","images/enemy1.png");
                            ("enemy2","images/enemy2.png");
                            ("enemy3","images/enemy3.png");
                            ("explosion","images/explosion.png");
                            ("fighter","images/fighter.png")
                            ("font","images/tom-thumb-white.png")
        ])
    let pixelFactor = (if mobile then 2.0 else 1.0)
    let width = ((float)width0/pixelFactor)
    let mutable entities = lazy(CreateEntityDB(this.Content, int width, int height))
    let fntImage = lazy(PIXI.Sprite(unbox this.Content?font?texture))
    let bgdImage = lazy(PIXI.Sprite(unbox this.Content?background?texture))
    let bgdRect = PIXI.Rectangle(0., 0., width, height)
    let scaleX = (float) (width / 320.) // pixelFactor
    let scaleY = (float) (height / 480.) // pixelFactor

    (** Draw the sprite for an Entity *)
    let drawSprite(spriteBatch:PIXI.Container) (entity) =
        match entity.Sprite with
        | Some sprite ->
            let scale =
                match entity.Scale with
                | Some(scale) -> scale
                | None -> PIXI.Point(1., 1.)
            let color = 
                match entity.Tint with 
                | Some(color) -> color
                | None -> Color.White
            sprite.x <- entity.Position.x
            sprite.y <- entity.Position.y
            sprite.scale <- scale
            sprite.tint <- float color
            spriteBatch.addChild(sprite) |> ignore

        | None -> ()

    member this.Initialize() =
        base.Initialize()

    override this.LoadContent() =
        entities.Force() |> ignore
        ()

    override this.Draw(gameTime) =        
        this.spriteBatch.children?length <- 0
        this.spriteBatch.addChild(bgdImage.Value) |> ignore
        ActiveEntities(entities.Value)
        |> List.sortBy(fun e -> e.Layer) 
        |> List.iter(drawSprite(this.spriteBatch))
        ()

    override this.Update(gameTime) =
        //  if GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed then 
        //     this.Exit()
        let delta = gameTime
        let current = entities.Value

        EnemySpawningSystem(delta, this) |> ignore
        entities <-  // Everything happens here:
            lazy (current
                 |> List.map(InputSystem(delta, mobile, this))
                 |> List.map(EntitySystem(this, int width, int height))
                 |> List.map(MovementSystem(delta))
                 |> List.map(ExpiringSystem(delta))
                 |> List.map(TweenSystem(delta, this))
                 |> List.map(RemoveOffscreenShipsSystem(this, int width, int height))
                 |> CollisionSystem(this)
                 )

        // pick up the list when we draw
        //this.entityList <- ActiveEntities (entities.Force())
       
    (** Deactivate an Entity *)
    override this.RemoveEntity(id:int) =
        this.Deactivate <- id :: this.Deactivate

    (** Que a Bullet *)
    override this.AddBullet(x: float, y:float) =
        this.Bullets <- BulletQue(x, y) :: this.Bullets
        //Browser.console.log("AddBullet", x, y, this.Bullets.Length)

    (** Que a Enemy *)
    override this.AddEnemy(enemy : Enemies) =
        match enemy with 
        | Enemies.Enemy1 -> this.Enemies1 <- EnemyQue(enemy) :: this.Enemies1
        | Enemies.Enemy2 -> this.Enemies2 <- EnemyQue(enemy) :: this.Enemies2
        | Enemies.Enemy3 -> this.Enemies3 <- EnemyQue(enemy) :: this.Enemies3
        | _ -> ()

    (** Que an Explosion *)
    override this.AddExplosion(x: float, y:float, scale : float) =
        this.Explosions <- ExplosionQue(x, y, scale) :: this.Explosions

    (** Que an Bang *)
    override this.AddBang(x: float, y:float, scale : float) =
        this.Bangs <- ExplosionQue(x, y, scale) :: this.Bangs


Keyboard.init()
Mouse.init()
let game = ShmupWarz(320.*1.5, 480.*1.5, false)
game.Run()


