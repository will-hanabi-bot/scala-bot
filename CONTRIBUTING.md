# Contribution Guide

Thanks for your interest in contributing! Every little bit helps.

## Bug report

Before opening a bug report:
- Verify the bot is playing with the expected settings.
	- On hanab.live, it writes a note containing its settings on the oldest card of the first player.
- Check whether a similar issue has already been reported. If so, leave a like or comment rather than opening a duplicate.
- Double-check convention docs.
- If a mistake is made earlier on in the game, the bot may behave erratically later on.
	- Leave a note if a mistake happened, even if potentially irrelevant.

To open a bug report:
1. In the [Issues section](https://github.com/will-hanabi-bot/scala-bot/issues), click `New issue` -> `Bug report`.
2. Fill out the description with information from a game where the bug occurred.
	- If on hanab.live, make sure to provide a replay link (contains `/replay/`) and not a table link (contains `/game/`).
3. Include any information that might be helpful, such as the relevant turns and surrounding context.
	- Follow-up questions may be asked to clarify details.

The issue will be closed when the bug is fixed, which can take anywhere from weeks to months depending on complexity.

## Feature request

Before opening a feature request, check whether a similar feature has already been requested. If so, leave a like or comment rather than opening a duplicate.

To open a feature request:
1. In the [Issues section](https://github.com/will-hanabi-bot/scala-bot/issues), click `New issue` -> `Blank issue`.
2. Fill out the description with your idea. The more specific the idea is, the easier it will be to implement.
	- Follow-up questions may be asked to clarify details.

Features related to mistake recovery and context are generally very complex and hard to implement, but if you have ideas, please share!

## Local development

- You'll need to have Scala 3. There are instructions [here](https://www.scala-lang.org/download/).
- Fork this repository and clone it to your computer.
- To verify the bot works, start it with `scala-cli . --main-class scala_bot.main -- index=<index>`.

scala-bot has several main modules:
- `main` connects to hanab.live and plays games with users
- `replay` loads a completed game and allows jumping between turns
- `selfPlay` plays with copies of itself on randomly-generated decks
- `analyze` loads a completed game and logs turns where it would have done something different

Debugging is typically done with the `replay` module. During a replay, logs will show up in the console, detailing what the bot thinks about every action. Two commands can be entered into the console while the program is running:
	- `hand <playerName> [observerName]` displays the information on that player's hand from a particular perspective.
		- If no observer name is provided, the hand is logged from the common knowledge perspective.
	- `navigate <turn>` travels to a specific turn.
		- If it is the bot's turn, it will provide a suggestion on what it would do.
		- Instead of a turn number, `+` (next turn), `++` (next turn of the same player), `-`, and `--` can also be used.

I've written a blog post explaining some of the features [here](https://willflame.mataroa.blog/blog/debugging-my-hanabi-bot/).

Once you have everything set up, feel free to tackle any of the existing issues (or your own ideas)! Any marked with "good first issue" are great places to start.

### Testing

There are a suite of tests in the `src/test/` folder.
- Tests in a particular folder can be run using `scala-cli test <path>`. To run all of them, use `scala-cli test .`.
- Pass `--test-only <testPattern>` to only run tests matching a particular pattern.
	- For example, `scala-cli test . --test-only "tests.hgroup.level8.PositionalMisplays*` only runs the tests in the `PositionalMisplays` class in `test/hgroup/level8.scala`.
- To run a particular test in a class, add `.only` to the test name.
	- For example, `test("doesn't eliminate from a possibly-fake finesse".only):`
- By default, debug logs are disabled during tests. To enable them, comment out the `beforeAll()` definition at the beginning of the class.

### Opening a pull request

Once you have a completed fix or feature, a pull request (PR) allows your change to be merged into this repository. Guidelines for PRs:
- Focuses on one topic and doesn't contain several unrelated changes
- PR description contains a list of the fixes/features
- All existing tests pass, and contains new tests verifying the changed functionality
- Code is organized and clean

Don't worry about getting everything perfect right away! A maintainer will review the PR and may suggest changes before it can be merged.

### Packaging and distribution

- Before packaging, set the compile log priority to 0 (in `logger.scala`) so that debug logs are removed from the outputted binary.
- `make build` will package a JAR file based on the current state of the repository.
- To reduce memory usage even further, GraalVM's [native-image](https://www.graalvm.org/latest/reference-manual/native-image/) can create an executable that doesn't require the JVM.
	- Once `native-image` is installed, add its path to the Makefile, then run `make build-native`.
	- Note that creating a native image is an expensive operation and requires a non-trivial amount of RAM.
