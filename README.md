# scala-bot

A deterministic *Hanabi* bot that plays on the [hanab.live](https://hanab.live/) interface. Basic structure and ideas were taken from [Zamiell's example bot](https://github.com/Zamiell/hanabi-live-bot) (Python). Fork of my [more-developed bot](https://github.com/will-hanabi-bot/hanabi-bot) (JavaScript), successor of the [Rust rewrite](https://github.com/will-hanabi-bot/rust-bot).

It can play with [Reactor 1.0](https://hanabi.wiki/conventions/reactor-1) (3p only), [Referential Sieve](https://hackmd.io/Ui6LXAK3TdC7AKSDcN20PQ?view) (with [special 2p](https://willflame14.github.io/rs-docs/)), or [H-Group](https://hanabi.github.io/) (up to level 11) conventions. Just like my other bots, it does not do any "learning" during the game.

A demo game using Reactor 1.0 conventions with the Critical Fours variant:

https://github.com/user-attachments/assets/665e7d78-7c8b-4f07-b4c3-9116ec2ec8ab

## Bot features

- Supports all the basic variants: Black, Rainbow, White, Prism, Pink, Brown, and blends.
- Takes notes during the game on what it thinks each player knows about their own hand.
- Can analyze completed games on hanab.live and offer suggested actions.

## Running locally
- Ensure you have [JVM 25](https://www.oracle.com/ca-en/java/technologies/downloads/#java25) or later.
- Download the latest JAR file from the [Releases page](https://github.com/will-hanabi-bot/scala-bot/releases).
- Fill out the login details for the bot in an .env file. See the example .env.template file and the [reference](#environment-variables-reference).
  - You'll need to create its account on hanab.live first.
- Run `java [JVM_OPTS] -jar scala-bot-<version>.jar index=<index>` to start the bot.
  - Suggested JVM options for reduced memory usage: `-Xms128m -Xmx192m -Xss256k -XX:MaxMetaspaceSize=96m -XX:ReservedCodeCacheSize=64m -XX:+UseSerialGC`.

## Contributing
Noticed a bug, have an idea for a feature, or want to help with development? See the [contribution guide](./CONTRIBUTING.md).

## Supported commands

Send a PM to the bot on hanab.live (`/pm <HANABI_USERNAME> <message>`) to interact with it.
- `/join [password]` to join your lobby.
- `/rejoin` to rejoin a game that has already started, if it had disconnected.
- `/leave` to kick the bot from your table.
- `/settings [convention]` to modify the convention set. For example, `/settings HGroup11` sets the bot to H-Group level 11.
  - Supported conventions: `RefSieve`, `Reactor1`, `HGroup[1-11]`
- `/analyze <replayId> <convention>` to provide a list of potential mistakes and suggested actions in the given replay.
- `/version` to get the version the bot is running.
- `/help` to get a link back to this page.

Some commands can be sent inside the room's chat to affect all bots that have joined.
- `/leaveall` to kick all bots from the table.
- `/setall` to set the same settings for all bots at the table.

## Watching replays

A replay from hanab.live or from a file (in JSON) can be loaded using `scala-cli . --main-class scala_bot.replay -- <options>`.
- `id=<id>` indicates the ID of the hanab.live replay to load.
- `file=<filePath>` indicates the path to the JSON replay to load (relative from the root directory).
- `index=<index>` sets the index of the player the bot will simulate as (defaults to 0).
- `convention=<convention>` sets the convention set (defaults to Reactor 1.0).

In a replay, the following commands are also supported (in addition to `hand`):
- `navigate <turn>` to travel to a specific turn.
    - If it is the bot's turn, it will provide a suggestion on what it would do.
    - Instead of a turn number, `+` (next turn), `++` (next turn of the same player), `-`, and `--` can also be used.

## Self-play
The bot can play games with copies of itself using `scala-cli . --main-class scala_bot.selfPlay [-- <options>]`. Possible options:
- `games=<numGames>` sets the number of games to play (defaults to 1).
- `seed=<seed>` sets the seed of the first game to be played (defaults to 0).
    - A seed can be a number or a string, but the seeding algorithm is different from the one used on hanab.live.
- `players=<numPlayers>` sets the number of players.
- `convention=<convention>` sets the convention set (defaults to Reactor 1.0).

The final score for each seed as well as how each game terminated are logged to the console. JSON replays of each game are saved to a `seeds` folder, which can be loaded into hanab.live for viewing.

## Environment variables reference

| Variable | Default | Description |
|---|---|---|
| `HANABI_LEAVE_PREGAME_IF_ONLY_BOTS` | `0` | Auto-leave the pregame lobby when only bots remain. |
| `HANABI_LEAVE_REPLAY_IF_ONLY_BOTS` | `1` | Auto-leave a shared replay when only bots are spectating. |
| `HANABI_BOT_NAME_PREFIXES` | *(empty)* | Comma-separated prefixes identifying bot accounts (e.g. `will-bot,mybot`). Required for the two flags above to take effect. |
