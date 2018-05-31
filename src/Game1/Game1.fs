namespace SM

open System

module Game1 =

    type Player = {
        name: string
        x: int
        y: int
        bulletCount: int
    }

    type Block = {
        x: int
        y: int
    }

    type Bullet = {
        x: int
        y: int
        xDelta: int
        yDelta: int
        moveIntervalMiliseconds: float
        lastMoveTime: DateTime
    }
    with
        member this.MaybeMove =
            let dtn = DateTime.Now
            match this.lastMoveTime with
            | t when t.AddMilliseconds this.moveIntervalMiliseconds >= dtn -> 
            { this with x = this.x + this.xDelta; y = this.y + this.yDelta; lastMoveTime = dtn }
            | _ -> this

    type World = {
        xMax: int
        yMax: int
        player1: Player Option
        player2: Player Option
        blocks: Block list
        bullets: Bullet list
        scoreP1: int
        scoreP2: int
    } 
    with
        member this.HasBlock x y =
            this.blocks
            |> List.exists (fun b -> b.x = x && b.y = y)

        member this.HasBullet x y =
            this.bullets
            |> List.exists   (fun b -> b.x = x && b.y = y)      

        member this.CanMove x y =
            match x, y with
            | x, y when x >= 0 && y >= 0 && x <= this.xMax && y <= this.yMax && this.HasBlock x y |> not -> true
            | _ -> false

        member this.Shoot x y xDelta yDelta =
            { this with bullets = { x = x + 1; y = y; xDelta = xDelta; yDelta = yDelta; lastMoveTime = DateTime.Now; moveIntervalMiliseconds = 1000. } :: this.bullets }
      

    let generateWorld xMax yMax = 
        { xMax = xMax; yMax = yMax; player1 = None; player2 = None; blocks = []; bullets = []; scoreP1 = 0; scoreP2 = 0 }

    let spawnPlayer1 x y (world: World) = 
        let pl = { name = "Player1"; x = x; y = y; bulletCount = 3 }
        { world with player1 = Some pl }

    let spawnPlayer2 x y (world: World) = 
        let pl = { name = "Player2"; x = x; y = y; bulletCount = 3 }
        { world with player2 = Some pl }

    let setBlock x y (world: World) =
        { world with blocks = { x = x; y = y} :: world.blocks}

    let setBlockProc p (world: World) : World =
        let rnd = System.Random()
        let blocks = 
            [0 .. world.xMax]
            |> List.collect (fun x -> 
                [0 .. world.yMax]
                |> List.map (fun y ->
                    if rnd.Next(100) < p then Some { x = x; y = y} 
                        else None
                    ))
            |> List.choose id
        { world with blocks = blocks }            

    let MaybeMoveBullets (world: World) =
        { world with bullets = world.bullets |> List.map (fun b -> b.MaybeMove) }

    let MaybeDisposeBullets (world: World) =
        { world with bullets = world.bullets |> List.filter (fun b -> b.x >= 0 &&  b.x <= world.xMax && b.y <= world.yMax) }    

    let MaybeHitP1 (world: World) =
        match world.player1 with
        | Some p when world.HasBullet p.x p.y -> 
            { world with scoreP2 = world.scoreP2 + 1 }
        | _ -> world

    let MaybeHitP2 (world: World) =
        match world.player2 with
        | Some p when world.HasBullet p.x p.y -> 
            { world with scoreP1 = world.scoreP1 + 1 }
        | _ -> world      

    type Direction =
    | Up
    | Down
    | Right
    | Left

    type Command =
    | MoveP1 of Direction
    | ShootP1
    | MoveP2 of Direction
    | ShootP2
    | GetWorldChannel of AsyncReplyChannel<World>

    let getWorld x y =
        generateWorld x y

    type Game (world: World) =
        // Game loop
        let gameAgent = MailboxProcessor.Start(fun inbox-> 
            let rec messageLoop oldWorld = async {
                let! msg = inbox.Receive() // Wait point

                let pl1 = oldWorld.player1.Value
                let pl2 = oldWorld.player2.Value
                let newWorld =
                    match msg with 
                        | MoveP1 Up when oldWorld.CanMove pl1.x (pl1.y + 1) -> { oldWorld with player1 = Some { pl1 with y = pl1.y + 1 } }
                        | MoveP1 Down when oldWorld.CanMove pl1.x (pl1.y - 1) -> { oldWorld with player1 = Some { pl1 with y = pl1.y - 1 } }
                        | MoveP1 Right when oldWorld.CanMove (pl1.x + 1) pl1.y -> { oldWorld with player1 = Some { pl1 with x = pl1.x + 1 } }
                        | MoveP1 Left when oldWorld.CanMove (pl1.x - 1) pl1.y -> { oldWorld with player1 = Some { pl1 with x = pl1.x - 1 } }
                        | ShootP1 -> oldWorld.Shoot pl1.x pl1.y 1 0
                        | MoveP2 Up when oldWorld.CanMove pl2.x (pl2.y + 1) -> { oldWorld with player2 = Some { pl2 with y = pl2.y + 1 } }
                        | MoveP2 Down when oldWorld.CanMove pl2.x (pl2.y - 1) -> { oldWorld with player2 = Some { pl2 with y = pl2.y - 1 } }
                        | MoveP2 Right when oldWorld.CanMove (pl2.x + 1) pl2.y -> { oldWorld with player2 = Some { pl2 with x = pl2.x + 1 } }
                        | MoveP2 Left when oldWorld.CanMove (pl2.x - 1) pl2.y -> { oldWorld with player2 = Some { pl2 with x = pl2.x - 1 } }
                        | ShootP2 -> oldWorld.Shoot (pl2.x - 1) pl2.y -1 0
                        | GetWorldChannel c -> c.Reply(oldWorld); oldWorld
                        | _ -> oldWorld // Nothing happened
                    |> MaybeMoveBullets
                    |> MaybeHitP1
                    |> MaybeHitP2
                    |> MaybeDisposeBullets

                return! messageLoop newWorld
            }
            world
            |> messageLoop
            )

        member __.UpP1 = MoveP1 Up |> gameAgent.Post
        member __.DownP1 = MoveP1 Down |> gameAgent.Post
        member __.LeftP1 = MoveP1 Left |> gameAgent.Post
        member __.RightP1 = MoveP1 Right |> gameAgent.Post
        member __.ShootP1 = ShootP1 |> gameAgent.Post
        member __.UpP2 = MoveP2 Up |> gameAgent.Post
        member __.DownP2 = MoveP2 Down |> gameAgent.Post
        member __.LeftP2 = MoveP2 Left |> gameAgent.Post
        member __.RightP2 = MoveP2 Right |> gameAgent.Post
        member __.ShootP2 = ShootP2 |> gameAgent.Post
        member __.GetWorld () = GetWorldChannel |> gameAgent.PostAndAsyncReply
