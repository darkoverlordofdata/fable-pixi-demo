[<AutoOpen>]
module EntityModule
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content
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
                                            (* *IEntity interface* *)
        Id              : int;              (* Unique sequential id *)
        Name            : string;           (* Display name *)
        Active          : bool;             (* In use *)
        EntityType      : EntityType;       (* Category *)
        Layer           : Layer;            (* Display Layer *)
        Position        : Vector2;          (* Position *)
        Sprite          : Texture2D option; (* Sprite *)
        Scale           : Vector2 option;   (* Display Scale *)
        Tint            : Color option;     (* Color to use as tint *)
                                            (* *Custom Items* *)
        Bounds          : int option;
        Expires         : float32 option;
        Health          : Health option;
        ScaleAnimation  : ScaleAnimation option;
        Size            : Vector2;          
        Velocity        : Vector2 option;
    }
    interface IEntity with
        member this.Id with get() = this.Id
        member this.Name with get() = this.Name
        member this.Active with get() = this.Active
        member this.EntityType with get() = int this.EntityType
        member this.Layer with get() = int this.Layer
        member this.Position with get() = this.Position
        member this.Scale with get() = this.Scale
        member this.Sprite with get() = this.Sprite
        member this.Tint with get() = this.Tint




(** Create a Player Entity *)
let CreatePlayer (content:ContentManager, x: float32, y: float32) =
    let position = Vector2(x, y)
    let sprite = content.Load<Texture2D>("images/fighter")
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
        Velocity = Some(Vector2(0.f, 0.f));
        ScaleAnimation = None;
        Size = Vector2(float32 sprite.Width, float32 sprite.Height);
    }
     
(** Create a Bullet Entity *)
let CreateBullet (content:ContentManager, x: float32, y: float32) =
    let position = Vector2(x, y)
    let sprite = content.Load<Texture2D>("images/bullet")
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
        Expires = Some(0.1f);
        Health = None;
        Velocity = Some(Vector2(0.f, -800.f));
        ScaleAnimation = None;
        Size = Vector2(float32 sprite.Width, float32 sprite.Height);
    }

(** Create Enemy *)
let CreateEnemy1 (content:ContentManager, width: int, height: int)  =
    let position = Vector2(float32(rnd.Next(width)), 100.f)
    let sprite = content.Load<Texture2D>("images/enemy1")
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
        Velocity = Some(Vector2(0.f, 40.f));
        ScaleAnimation = None;
        Size = Vector2(float32 sprite.Width, float32 sprite.Height);
    }

(** Create Enemy *)
let CreateEnemy2 (content:ContentManager, width: int, height: int) =
    let position = Vector2(float32(rnd.Next(width)), 200.f)
    let sprite = content.Load<Texture2D>("images/enemy2")
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
        Velocity = Some(Vector2(0.f, 30.f));
        ScaleAnimation = None;
        Size = Vector2(float32 sprite.Width, float32 sprite.Height);
    }

(** Create Enemy *)
let CreateEnemy3 (content:ContentManager, width: int, height: int)  =
    let position = Vector2(float32(rnd.Next(width)), 300.f)
    let sprite = content.Load<Texture2D>("images/enemy3")
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
        Velocity = Some(Vector2(0.f, 20.f));
        ScaleAnimation = None;
        Size = Vector2(float32 sprite.Width, float32 sprite.Height);
    }


(** Create Big Explosion *)
let CreateExplosion (content:ContentManager, x: float32, y: float32, scale:float32) =
    let position = Vector2(x, y)
    let sprite = content.Load<Texture2D>("images/explosion")
    {
        Id = NextUniqueId();
        Name = "Explosion";
        Active = false;
        EntityType = EntityType.Explosion; 
        Layer = Layer.EXPLOSION;
        Position = position; 
        Scale = Some(Vector2(scale, scale))
        Sprite = Some(sprite);
        Tint = Some(Color.LightGoldenrodYellow);

        Bounds = None;
        Expires = Some(0.5f);
        Health = None;
        Velocity = None;
        ScaleAnimation = Some(CreateScaleAnimation(scale/100.f, scale, -3.f, false, true));
        Size = Vector2(float32 sprite.Width, float32 sprite.Height);
    }

let CreateBang (content:ContentManager, x: float32, y: float32, scale:float32) =
    let position = Vector2(x, y)
    let sprite = content.Load<Texture2D>("images/bang")
    {
        Id = NextUniqueId();
        Name = "Bang";
        Active = false;
        EntityType = EntityType.Explosion; 
        Layer = Layer.BANG;
        Position = position; 
        Scale = Some(Vector2(scale, scale))
        Sprite = Some(sprite);
        Tint = Some(Color.PaleGoldenrod);

        Bounds = None;
        Expires = Some(0.5f);
        Health = None;
        Velocity = None;
        ScaleAnimation = Some(CreateScaleAnimation(scale/100.f, scale, -3.f, false, true));
        Size = Vector2(float32 sprite.Width, float32 sprite.Height);
    }

(**
 * Create the Entity DataBase
 *) 
let CreateEntityDB(content, width, height) = 
    [
        CreatePlayer(content, float32(width/2), float32 (height-80));
        CreateBang(content, 0.f, 0.f, 1.f);
        CreateBang(content, 0.f, 0.f, 1.f);
        CreateBang(content, 0.f, 0.f, 1.f);
        CreateBang(content, 0.f, 0.f, 1.f);
        CreateBang(content, 0.f, 0.f, 1.f);
        CreateBang(content, 0.f, 0.f, 1.f);
        CreateBang(content, 0.f, 0.f, 1.f);
        CreateBang(content, 0.f, 0.f, 1.f);
        CreateExplosion(content, 0.f, 0.f, 1.f);
        CreateExplosion(content, 0.f, 0.f, 1.f);
        CreateExplosion(content, 0.f, 0.f, 1.f);
        CreateExplosion(content, 0.f, 0.f, 1.f);
        CreateExplosion(content, 0.f, 0.f, 1.f);
        CreateExplosion(content, 0.f, 0.f, 1.f);
        CreateExplosion(content, 0.f, 0.f, 1.f);
        CreateExplosion(content, 0.f, 0.f, 1.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
        CreateBullet(content, 0.f, 0.f);
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

