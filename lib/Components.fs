[<AutoOpen>]
module ComponentsModule
open System
// open Microsoft.Xna.Framework
// open Microsoft.Xna.Framework.Graphics
// open Microsoft.Xna.Framework.Content
(**
 * Component Factory
 *
 *)


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

(** ScaleAnimation Component *)
type ScaleAnimation =
    {
        Min : float32;
        Max : float32;
        Speed : float32;
        Repeat : bool;
        Active : bool;
    }
(** Create a Scale Animation Component *)
let CreateScaleAnimation(min: float32, max: float32, speed: float32, repeat: bool, active: bool) =
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
        X: float32;
        Y: float32;
        Scale : float32;
    }
let ExplosionQue(x:float32, y:float32, scale : float32) : ExplosionQueItem =
    {
        X = x;
        Y = y;
        Scale = scale;
    }

(** Request a bullet *)
type BulletQueItem =
    {
        X: float32;
        Y: float32;
    }
let BulletQue(x:float32, y:float32) : BulletQueItem =
    {
        X = x;
        Y = y;
    }
    


