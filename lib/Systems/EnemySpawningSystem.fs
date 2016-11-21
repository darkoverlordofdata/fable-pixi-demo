[<AutoOpen>]
module EnemySpawningSystem

open Microsoft.Xna.Framework

type Timers =
    | Timer1 = 2
    | Timer2 = 7
    | Timer3 = 13

let mutable enemyT1 = float32(Timers.Timer1)
let mutable enemyT2 = float32(Timers.Timer2)
let mutable enemyT3 = float32(Timers.Timer3)

let EnemySpawningSystem (delta:float32, game:EcsGame)  =

    //let igame = game:>IGame
    let spawnEnemy (t:float32, enemy) =
        let delta = t - delta

        if delta < 0.0f then
            game.AddEnemy(enemy)
            match enemy with
            | Enemies.Enemy1 -> float32(Timers.Timer1)
            | Enemies.Enemy2 -> float32(Timers.Timer2)
            | Enemies.Enemy3 -> float32(Timers.Timer3)
            | _ -> 0.0f
        else delta

    enemyT1 <- spawnEnemy(enemyT1, Enemies.Enemy1)
    enemyT2 <- spawnEnemy(enemyT2, Enemies.Enemy2)
    enemyT3 <- spawnEnemy(enemyT3, Enemies.Enemy3)
    

    

