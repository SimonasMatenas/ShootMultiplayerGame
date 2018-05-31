module Test1

open Fable.Core
open Fable.Import.Browser
open SM
open SM.Game1

let borderSize = 5.
let xMax = 60
let yMax = 30
let tileSize = 10.
let gameAreaSize = ((borderSize * 2.) + float xMax * tileSize, (borderSize * 2.) + float yMax * tileSize)

let canvas =  document.getElementsByTagName_canvas().[0]
canvas.width <- fst gameAreaSize + (borderSize * 2.)
canvas.height <- snd gameAreaSize + (borderSize * 2.) + 25.

let wallTop   = ([0.0..borderSize..(fst gameAreaSize)] |> List.map (fun x-> (x + borderSize,0.,borderSize,borderSize)))
let wallDown  = ([0.0..borderSize..(fst gameAreaSize)] |> List.map (fun x-> (x + borderSize,(snd gameAreaSize) + borderSize,borderSize,borderSize)))
let wallLeft  = ([0.0..borderSize..(snd gameAreaSize) + borderSize] |> List.map (fun x-> (0.,x,borderSize,borderSize)))
let wallRight = ([0.0..borderSize..(snd gameAreaSize)] |> List.map (fun x-> ((fst gameAreaSize) + borderSize,x + borderSize,borderSize,borderSize)))
let wall = wallLeft @ wallTop @ wallRight @ wallDown

let drawWall (wall:(float*float*float*float) List) =
  let ctx = canvas.getContext_2d()
  wall |> List.iter (fun x->
      ctx.fillStyle <- U3.Case1 "black"
      match x with
      | x,y,w,h -> ctx.fillRect(x, y, w, h)
  ) |> ignore

let writeStats (world: Game1.World) =
  let ctx = canvas.getContext_2d()
  ctx.fillStyle <- U3.Case1 "red"
  ctx.font <- "18px Segoe UI";
  ctx.clearRect(0., snd gameAreaSize  + (borderSize * 2.), fst gameAreaSize, 25.) |> ignore
  let text = if world.player1.IsSome then sprintf "Bullets - %i" world.player1.Value.bulletCount else "No player"
  ctx.fillText(text, borderSize, (snd gameAreaSize) + 25.) |> ignore
  // P2
  let text = if world.player2.IsSome then sprintf "Bullets - %i" world.player1.Value.bulletCount else "No player"
  let metrics = ctx.measureText(text)
  ctx.fillText(text, fst gameAreaSize - metrics.width, (snd gameAreaSize) + 25.) |> ignore
  // Score
  let text = sprintf "%i ----- %i" world.scoreP1 world.scoreP2
  let metrics = ctx.measureText(text)
  ctx.fillText(text, fst gameAreaSize / 2. - metrics.width / 2., (snd gameAreaSize) + 25.) |> ignore

let drawBullet (b: Game1.Bullet) =
  let ctx = canvas.getContext_2d()
  ctx.fillStyle <- U3.Case1 "red"
  ctx.fillRect(float b.x * tileSize + borderSize, float b.y * tileSize + borderSize, tileSize * 0.8, tileSize * 0.8)

let drawBlock (b: Game1.Block) =
  let ctx = canvas.getContext_2d()
  ctx.fillStyle <- U3.Case1 "grey"
  ctx.fillRect(float b.x * tileSize + borderSize + tileSize * 0.15, float b.y * tileSize + borderSize + tileSize * 0.15, tileSize * 0.7, tileSize * 0.7)

let drawPlayer color (pl: Player Option)  =
  let ctx = canvas.getContext_2d()
  ctx.fillStyle <- U3.Case1 color
  match pl with
  | Some p -> ctx.fillRect(float p.x * tileSize + borderSize, float p.y * tileSize + borderSize, tileSize, tileSize)
  | _ -> ()

let drawWorld (world: Game1.World) =
  world.player1 |> drawPlayer "blue"
  world.player2 |> drawPlayer "green"
  world.bullets |> List.iter drawBullet
  world.blocks |> List.iter drawBlock
  writeStats world

let rec update (game: Game) () : Async<unit> = async {
  let ctx = canvas.getContext_2d()
  ctx.clearRect(borderSize, borderSize, fst gameAreaSize, snd gameAreaSize) // Avoid reset the wall
  let! world = game.GetWorld ()
  drawWorld world
  do! Async.Sleep 50
  return! update game ()
  }
  
let main() =
  let world = 
    Game1.generateWorld xMax yMax
    |> Game1.setBlockProc 10
    |> Game1.spawnPlayer1 2 2
    |> Game1.spawnPlayer2 (xMax - 2)  (yMax - 2)

  let game = Game(world)

  window.addEventListener_keydown(fun e ->
      // P1
      if e.keyCode = 65. then game.LeftP1
      if e.keyCode = 87. then game.DownP1
      if e.keyCode = 68. then game.RightP1
      if e.keyCode = 83. then game.UpP1
      if e.keyCode = 32. then game.ShootP1
      // P2
      if e.keyCode = 37. then game.LeftP2
      if e.keyCode = 38. then game.DownP2
      if e.keyCode = 39. then game.RightP2
      if e.keyCode = 40. then game.UpP2
      if e.keyCode = 96. then game.ShootP2
    :> obj)

  drawWall wall
  update game () |> Async.StartImmediate // rec

main()