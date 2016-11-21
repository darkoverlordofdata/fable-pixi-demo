[<AutoOpen>]
module EntitiesModule
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content

(**
 * The abstract EscGame provides interface and lists to
 * use for adding and removing entities
 * This allows systems to hold a forward reference to the game object
 *)
[<AbstractClass>]
type EcsGame(height, width, mobile)=
    inherit AbstractGame(height, width, mobile)
    //inherit Game()

    member val Bullets = List.empty<BulletQueItem> with get,set
    member val Deactivate = List.empty<int> with get,set
    member val Enemies1 = List.empty<EnemyQueItem> with get,set
    member val Enemies2 = List.empty<EnemyQueItem> with get,set
    member val Enemies3 = List.empty<EnemyQueItem> with get,set
    member val Explosions = List.empty<ExplosionQueItem> with get,set
    member val Bangs = List.empty<ExplosionQueItem> with get,set

    abstract member AddBullet : float32 * float32 -> unit
    abstract member AddEnemy : Enemies -> unit 
    abstract member AddExplosion : float32 * float32 * float32 -> unit
    abstract member AddBang : float32 * float32 * float32 -> unit
    abstract member RemoveEntity: int -> unit


(**
 * Returns a list of active entities for drawing.
 * No need to rev the returned list, they will be sorted by layer.
 *)
 let ActiveEntities (input:Entity list) =
    let rec _activeEntities (input:Entity list) (output:IEntity list) =
        match input with
        | x::xs when x.Active -> _activeEntities xs ((x:>IEntity)::output)
        | _::xs -> _activeEntities xs output 
        | [] -> output
    _activeEntities input []
