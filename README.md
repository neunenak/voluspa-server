High-Level Overview
=============

- Server state (in an `MVal`):
    - map of client IDs to `WS.Connection`s
    - map of game IDs to `Game`s (for now, a game is just a pair of client IDs -- in the future, we could put more game state server-side)
    - list (or a Maybe, since there won't be more than 1) of waiting client IDs

- When a client connects,
    - assign it a random client ID and drop it into the client map

- When a client sends a message, parse it as JSON. Then,
    - if the `type` is `GameStart`,
        - if there are no waiting clients,
            - assign the client to the waiting list
        - if there is a waiting client,
            - create a new game with a random game ID between these two clients
            - add the game to the games map
            - send a `GameStarted` message to both clients with the `state` that was in the client message and the generated `gameId`
    - otherwise,
        - find the matching opponent client ID (from the message's `gameId`)
        - find the opponent's corresponding `Connection`
        - forward the whole message to the opponent

- When a client disconnects,
    - ???

- When a game ends,
    - ???