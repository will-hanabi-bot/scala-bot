# scala-bot

A deterministic Scala bot that plays on the [hanab.live](https://hanab.live/) interface. Basic structure and ideas were taken from [Zamiell's example bot](https://github.com/Zamiell/hanabi-live-bot) (Python). Fork of my [more-developed bot](https://github.com/will-hanabi-bot/hanabi-bot) (JavaScript), successor of the [Rust rewrite](https://github.com/will-hanabi-bot/rust-bot).

It can play with [Reactor 1.0](https://hanabi.wiki/en/conventions/reactor) (3p only), [Referential Sieve](https://hackmd.io/Ui6LXAK3TdC7AKSDcN20PQ?view), or [H-Group](https://hanabi.github.io/) (up to level 2) conventions. Just like my other bots, it does not do any "learning" during the game.

## Bot features

- Supports all the basic variants (Black, Rainbow, White, Prism, Pink, Brown + blends).
- Takes notes during the game on what it thinks each player knows about their own hand.
- Can replay completed games on hanab.live and offer suggested actions.

## Running locally

- You'll need to have Scala and Coursier installed. There are instructions [here](https://www.scala-lang.org/download/).
- Clone the repository to your own computer. There are lots of tutorials online on using Git if you don't know how that works.
- Navigate to the cloned repository in a terminal.
- Fill out the login details for the bot in an .env file. See .env.template for an example.
  - You'll need to create its account on hanab.live first.
- Run `scala-cli . --main-class scala_bot.main -- index=<index>` to start the bot.
- Debug logs will show up in the console, providing more information about what the bot thinks about every action.
	- `hand <playerName> [observerName]` will display the information on that player's hand from a particular perspective.
        - If no observer name is provided, the hand will be logged from the common knowledge perspective.

## Supported commands

Send a PM to the bot on hanab.live (`/pm <HANABI_USERNAME> <message>`) to interact with it.
- `/join [password]` to join your current lobby. The bot will remain in your table until it is kicked with `/leave`.
- `/rejoin` to rejoin a game that has already started (e.g. if it crashed).
- `/leave` to kick the bot from your table.
- `/settings [convention=Reactor,RefSieve,HGroup] [level=1,2]` to modify the convention set.
- `/version` to get the current version of the bot.

Some commands can be sent inside a room to affect all bots that have joined.
- `/leaveall` to kick all bots from the table.
- `/setall` to set the same settings for all bots at the table.

## Watching replays

A replay from hanab.live or from a file (in JSON) can be simulated using `scala-cli . --main-class scala_bot.replay -- <options>`.
- `id=<id>` indicates the ID of the hanab.live replay to load.
- `file=<filePath>` indicates the path to the JSON replay to load (relative from the root directory).
- `index=<index>` sets the index of the player the bot will simulate as (defaults to 0).
- `convention=<conventionName>` sets the convention set.
	- `level=<level>` sets the level of the convention set (defaults to 1, if applicable).

In a replay, the following commands are also supported (in addition to `hand`):
- `navigate <turn>` to travel to a specific turn.
    - If it is the bot's turn, it will provide a suggestion on what it would do.
    - Instead of a turn number, `+` (next turn), `++` (next turn of the same player), `-`, and `--` can also be used.

## Self-play
The bot can play games with copies of itself using `cargo run --release --bin self_play [-- <options>]`. Possible options:
- `games=<numGames>` sets the number of games to play (defaults to 1).
- `seed=<seed>` sets the seed of the first game to be played (defaults to 0).
    - The seeding algorithm is different from the one used on hanab.live.
- `players=<numPlayers>` sets the number of players.
- `convention=<conventionName>` sets the convention set.
	- `level=<level>` sets the level of the convention set (defaults to 1, if applicable).

The final score for each seed as well as how each game terminated are logged to the console. JSON replays of each game are saved to a `seeds` folder, which can be loaded into hanab.live for viewing.
