[<AutoOpen>]
module RemoveOffscreenShipsSystem
open Microsoft.Xna.Framework

let RemoveOffscreenShipsSystem (game:EcsGame, width: int, height: int) entity =
    match entity.EntityType, entity.Active with
    | EntityType.Enemy, true when int entity.Position.Y > height ->
        { 
            entity with 
                Active = false;
        }
    | _ -> entity
