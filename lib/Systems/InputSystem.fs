[<AutoOpen>]
module InputSystem
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Input.Touch

let mutable timeToFire = 0.0f
(** Get Player Input *)
let InputSystem (kbState:KeyboardState, msState:MouseState, tcState:TouchCollection, delta:float32, mobile:bool, game:EcsGame) entity =

    let pf = if mobile then 2.0f else 1.0f
    let rec HandleKeys keys =
        match keys with
        | [] -> 
            0
        | x :: xs ->
            match x with
            | Keys.Z -> 
                timeToFire <- timeToFire - delta
                if timeToFire <= 0.0f then
                    game.AddBullet(entity.Position.X-27.f, entity.Position.Y)
                    game.AddBullet(entity.Position.X+27.f, entity.Position.Y)
                    timeToFire <- 0.1f
                HandleKeys xs 
            | _ -> 
                HandleKeys xs 

    match entity.EntityType with
    | EntityType.Player -> 

        let position =
            if tcState.Count > 0 then
                let p = tcState.[0].Position
                let newPosition = Vector2(float32 (p.X/pf), float32 (p.Y/pf))
                timeToFire <- timeToFire - delta
                if timeToFire <= 0.0f then
                    game.AddBullet(newPosition.X-27.0f, newPosition.Y)
                    game.AddBullet(newPosition.X+27.0f, newPosition.Y)
                    timeToFire <- 0.1f
                newPosition


            else
                HandleKeys(kbState.GetPressedKeys() |> Array.toList) |> ignore
                let newPosition = Vector2(float32 (msState.X), float32 (msState.Y))
                match msState.LeftButton with
                | ButtonState.Pressed ->
                    timeToFire <- timeToFire - delta
                    if timeToFire <= 0.0f then
                        game.AddBullet(float32 (msState.X-27), float32 msState.Y)
                        game.AddBullet(float32 (msState.X+27), float32 msState.Y)
                        timeToFire <- 0.1f
                    newPosition

                | ButtonState.Released ->
                    newPosition

                | _ ->
                    newPosition

        (* Set Player Position *)
        { entity with Position = position;  }

    | _ -> entity
