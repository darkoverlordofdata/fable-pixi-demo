[<AutoOpen>]
module EntitySystem
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

(**
 * Activate / Deactiveate Entities as needed 
 *)

let EntitySystem (game:EcsGame, width: int, height: int) entity =

    match entity.Active with
    | true -> 
        let mutable removed = false
        let rec remove_if l predicate =
            match l with
            | [] -> []
            | x::rest -> 
                if predicate(x) then 
                    removed <- true
                    (remove_if rest predicate) 
                else 
                    x::(remove_if rest predicate)

        let len = game.Deactivate.Length
        game.Deactivate <- (remove_if game.Deactivate (fun id -> id = entity.Id))
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
                        Expires = Some(0.5f);                        
                        Position = Vector2(bullet.X, bullet.Y);
                }
        | Layer.ENEMY1 ->
            match game.Enemies1 with
            | [] -> entity
            | enemy :: rest ->
               game.Enemies1 <- rest
               {
                    entity with 
                        Active = true;
                        Position = Vector2(float32(rnd.Next(width-35)), 91.f/2.0f);
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
                        Position = Vector2(float32(rnd.Next(width-86)), 172.f/2.0f);
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
                        Position = Vector2(float32(rnd.Next(width-160)), 320.f/2.0f);
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
                        Expires = Some(0.2f);                        
                        Scale = Some(Vector2(exp.Scale, exp.Scale));
                        Position = Vector2(exp.X, exp.Y);
                }
        | Layer.BANG ->
            match game.Bangs with
            | [] -> entity
            | exp :: rest ->
                game.Bangs <- rest
                {
                    entity with 
                        Active = true;
                        Expires = Some(0.2f);                        
                        Scale = Some(Vector2(exp.Scale, exp.Scale));
                        Position = Vector2(exp.X, exp.Y);
                }
        | _ -> entity


