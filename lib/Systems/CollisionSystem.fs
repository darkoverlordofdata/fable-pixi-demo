[<AutoOpen>]
module CollisionSystem
open Microsoft.Xna.Framework

(** Return Rect defining the current bounds *)
let BoundingRect(entity) =
    let x = (int entity.Position.X)
    let y = (int entity.Position.Y)
    let w = (int entity.Size.X)
    let h = (int entity.Size.Y)
    Rectangle(x - w/2, y - h/2, w, h)

(** Collision Handler for Entities *)
let CollisionSystem (game:EcsGame) entities =

    let FindCollision a b =
        match a.EntityType, a.Active, b.EntityType, b.Active with
        | EntityType.Enemy, true, EntityType.Bullet, true -> 
            //game.AddExplosion(b.Position.X, b.Position.Y, 0.25f)
            game.AddBang(b.Position.X, b.Position.Y, 1.0f)
            game.RemoveEntity(b.Id)
            match a.Health with
            | Some(h) ->
                let health = h.CurHealth-1
                if health <= 0 then
                    game.AddExplosion(b.Position.X, b.Position.Y, 0.5f)
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

    let rec FigureCollisions (entity:Entity) (sortedEntities:Entity list) =
        match sortedEntities with
        | [] -> entity
        | x :: xs -> 
            let a = if (BoundingRect(entity).Intersects(BoundingRect(x))) then
                        FindCollision entity x
                    else
                        entity
            FigureCollisions a xs

    let rec FixCollisions (toFix:Entity list) (alreadyFixed:Entity list) =
        match toFix with
        | [] -> alreadyFixed
        | x :: xs -> 
            let a = FigureCollisions x alreadyFixed
            FixCollisions xs (a::alreadyFixed)

    FixCollisions entities []


