Voluspa Server
==============

This is a simple Haskell websocket server for [Voluspa](https://github.com/AlexNisnevich/voluspa) using the [websockets](http://jaspervdj.be/websockets/index.html) library. It pairs clients together, passes appropriate game start messages, forwards messages from clients to their opponents, and handles disconnections gracefully.