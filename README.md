# Λbyrinth

## Scenes

The game consists of 3 **scenes**. A scene is just a screen that serves a specific purpose.
The scenes are, in order:

1. Create character
2. Game
3. Result

They show in succession, such that the player can create their character, play the game, and view how far they got.

## About the monad stack

### Brick's API and its limits

Brick exposes a super handy API for working with program state, and state transitions.
However, this implementation has its limits.
In the scene for the game itself, the Brick app is represented by the following type:

```haskell
app :: Scene GameState ≡ App GameState () Name
```

Now, all of `app`'s fields are dependent upon the type arguments one provides to the `App` type.
With the above implementation, all of `app`'s constituents have a well-defined type (which we cannot change).
The issue arises...

### Problem

> How can we implement monads over `GameState` without severely compromising code quality?

#### Solution 1: Just make the game state a monad

How about doing something like

```haskell
type Scene a = forall m. Monad m => ReaderT Config m a
```

Well, though it would technically _work_, this has a few problems:

- **Lots of refactoring!** Anything that touches the game state needs to be rewritten to more convoluted code
- **Not a common approach**: I searched a fair bit, and found no programs using Brick with this approach

#### Solution 2: Tetris-inspired monads

While learning Brick, I have used [this](https://github.com/SamTay/tetris) project.
It uses a series of custom type aliases to model a `StateT` on top of the `GameState`.
This seemed nice, but the eventual implementation had the following issues:

- The Tetris game model is different from mine, requiring severe refactoring
- The implementation did not solve my problem
- The implementation was quite overengineered for my needs

#### Solution 3: Inject monads on top of the `EventM` type

Yes! We did it!
Though I found no direct guidance for this approach, the following threads hinted me to this approach:

- [Reddit](https://www.reddit.com/r/haskell/comments/gmn78x/combining_brick_and_sbv_monadic_contexts/)
- [StackOverflow](https://stackoverflow.com/questions/71782417/getting-current-time-during-a-brickevent-haskell)

We can thus create a monad stack over `EventM` (which, notably is a monad), for the following type:

```haskell
type GameEvent a = ReaderT Config (EventM Name GameState) a
```

This fits _super well_ with my event-based approach!
With this approach, we can access the entire monad stack in each `GameEvent a`.

At the time of writing, we only use `ReaderT`, but there may be more monads at a later point.
This allows us to get, for example, difficulty setting and ASCII-only mode from the environment in all game events.

##### Limitations

There is, however, one limitation that I were not able to work around.
Aything _outside_ of the game events cannot access the `Reader` environment.
One such function is `appDraw`, which naturally needs to know whether the `ASCII-only` flag is enabled.
We work around this by explicitly passing the flag into the function.

## Running tests

In this project, we use `QuickCheck` for property based testing.
The test suite is quite limited, but demonstrates a setup and testing the properties of a binary tree.

To run tests, you can either:

- Run `cabal test --enable-tests`, which will run tests _once_
- Run `cabal configure --enable-tests` in order to permanently enable tests.
  This creates a `cabal.project.local` file (which is ignored by Git)

Thanks to [James](https://github.com/yobson/QuickCheck-Example/blob/main/) for the demo!
