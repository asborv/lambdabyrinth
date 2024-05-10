---
tags:
  - assignment
  - inf221
status: submitted
date: 2024-05-10
task: https://mitt.uib.no/courses/45746/assignments/88173
---

## Summary of the project

### The initial idea

This project, *Lambdabyrinth*, is a game heavily inspired by [Rogue](https://en.wikipedia.org/wiki/Rogue_(video_game)) and other derivatives. It is a terminal-based game in which the player progresses through a labyrinth. The labyrinth consists of several *level*, which are lazily evaluated at runtime. This way, we get an infinite game which keeps going until the player dies.

### Reason for choosing this project

I have always been fascinated by terminal programs like [Ranger](https://github.com/ranger/ranger) and GHCup. When I heard about *Brick*, I knew at once I wanted a TUI project of some kind. I considered a resource monitor or the web browser, but ultimately landed on creating a game to allow myself get a little silly and creative. Who doesn't love emojis?? (More on this later).

## Description of the program

### Game mechanics

*Lambdabyrinth* consists of 3 *scenes*;

1. Create your character
2. Play the game with created character
3. View game results once you die

Essentially, each *scene* is its own *Brick* `App`, passing their resulting states to the next app.
See how nicely each scene consumes the result of the previous one.
This is all tied together in a rather short `main` function in `Main.hs`.

### Scenes overview

#### Character creation

Once you start the game, you will be greeted by the following:

```.
┌────────────────Create character────────────────┐
│Name <enter name by typing>                     │
│                                                │
│Class          [*] Wizard                       │
│               [ ] Rogue                        │
│               [ ] Warrior                      │
│                                                │
└────────────────────────────────────────────────┘
```

- The `Name` field is active when the app starts, so you can start writing right away.
- Press `Tab` to switch between fields
- Scroll through the `Class` options with the arrow keys, and select with `Space`
- Click `Enter` to start the game

#### Game

Once you start the game, you will see something like the following:

![[Pasted image 20240510233811.png]]

- Navigate the world with `WASD`
- Attack monsters by walking into them. But careful, they will bite back!
- Your goal for each level is to reach the stairs down to the next level
- Chests may contain useful items. Walk into them to open. You will equip the item automatically if it is better than the currently equipped one.

##### Panels

- **Log**: left side of your screen. A log of what happens in the game. Very handy help as the graphics may require some creativity to understand.
- **Main screen**: the big one in the middle. Here's where the action goes down!
- **Equipment**: right side of your screen. This shows your currently equipped armour and weapons.
- **Health**: bottom of your screen.

##### Explanation of icons

| Emoji        | ASCII | Explanation                            |
| ------------ | ----- | -------------------------------------- |
|              | V     | Stairs down to the next level          |
|              | Λ     | Stairs up to the previous level        |
|              | .     | Floor                                  |
|              | #     | Walls                                  |
| 📫           | []    | Chest (closed, may contain treasures!) |
| 📭           | ()    | Chest (open)                           |
| 💎           | D     | Diamond                                |
| 🪨           | S     | Stone                                  |
| 🪵           | W     | Wood                                   |
| 🗡️           | -     | Dagger                                 |
| 🔱           | /     | Spear                                  |
| 🧙 / 🦹 / ⚔️  | P     | Player                                 |
| 🧟           | Z     | Zombie                                 |
| 👻           | G     | Ghost                                  |
| 🪖           | ^     | Helmet                                 |
| 🛡️           | #     | Cuirass                                |
| 🧤           | ''    | Gloves                                 |
| 👢           | ,,    | Boots                                  |

#### Results (game over)

```
┌───────────Results──────────┐
│                            │
│         YOU DIED!          │
│   Asbjørn got to level 0   │
│   Press any key to quit    │
│                            │
└────────────────────────────┘
```

When you die, you die. Game over. No getting it back.
You can marvel at the level you got to on this screen, and quit the game.

### Building locally

#### Platform support

I have mainly been developing this project on MacOs, but it also builds and runs fine on my Windows computer.
I have not tried Linux myself, but setup [Github actions](https://github.com/asborv/lambdabyrinth/actions) which build just fine. The last failed runs are because I used my entire quota.

> [!NOTE] Versions
> I have verified this project using:
> - GHC 9.2.8
> - Cabal 3.10.3.0

#### First time setup

TL;DR:

- Clone the repo: `git clone git@github.com:asborv/lambdabyrinth.git`
- Navigate to folder: `cd lambdabyrinth`
- Build project with Cabal: `cabal build`
- Play the game: `cabal run [lambdabyrinth -- <flags>]`

This is a Cabal project, so building and running it should be as easy as running `cabal run` in the root of the project. You can also pass arguments for the game through cabal with `cabal run lambdabyrinth -- <flags>`. At time of writing, supported flags are:

```bash
lambdabyrinth [-a | --ascii-only] [-d | --difficulty DIFFICULTY]
```

, where `DIFFICULTY = Easy | Medium | Hard`. More on this later.

#### Enabling tests

Apparently, the test suite is not enabled by default. Here are the options for building and running tests:

- `cabal build --enable-tests`: Compile the program and tests
- `cabal test --enable-tests`: Compile and run tests
- `cabal configure --enable-tests`: Permanently enable tests
	- This adds the file `cabal.project.local`, which isn't tracked by Git
	- **This is the recommended option**

### Half-baked features

As with any project, there are things that I would have liked to work a little more with. Here is a list:

- Monsters do not move yet. This was part of the MVP, but not prioritised during development. They do everything else a monster should do, so I don't take this as a great loss.
- Full-fledged inventory system and more general items
- User-friendly UI; especially when running in `--ascii-only` mode, it is difficult to make out the different entities on the game map

## Functional programming techniques used

In this chapter, I shall explain the functional programming techniques (neat Haskell-y stuff) I've done throughout this project.

### Parsing with `optparse-appliccative`

The file `Config.hs` defines the command line arguments one can pass to *Lambdabyrinth*.
The configuration is stored in the `Config` type (more on this [[#Game events with monad stacks | later]]).
I have used `optparse-appliccative` to write a parser for game configuration.
More specifically, the sub-parsers are:

```haskell
asciiFlag        :: Parser Bool -- parser for whether to use emojis or ASCII-art
difficultyOption :: Parser Difficulty -- parser for the game's difficulty
```

These are combined into a configuration parser which uses `optparse-appliccative`'s applicative API for parser combinations:

```haskell
Config <$> asciiFlag <*> difficultyOption
```

What a fantastic library and way to do this! This module is easily expandable with more parsers in the future.

### Game events with monad stacks

From the very start of the project, I was of the impression that I would need a monad stack.
Surprisingly, it took a long time before I actually needed it, much because *Brick* (in tandem with *Lenses*) provide so much functionality for free regarding `State`. Their APIs are both super well designed, and play together really well!

Now, eventually, I wanted to add the `--ascii-only` flag to my game, as well as a difficulty option.
This is the job for a `Reader`! I have written the backstory and competing solutions in the `README`, please take a look.

> [!NOTE] Aside: why the `--ascii-only` flag?
> It turns out that emojis are *difficult*. Early on, I discovered a bug where the borders on the same lines as an emoji would get pushed around. This is a [well known issue](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#wide-character-support-and-the-textwidth-class). I was able to work around this with writing strange backspace-infused strings for rendering emojis. They work well on my computer, but there are no guarantees that it will on yours. Some terminals straight up refuse to show emojis at all.
> Adding this flag both enabled me to provide a solution for this, as well as demonstrating an applicative parser!

#### Implementation

The solution ultimately ended up being wrapping *Brick*'s `EventM` into my own monad stack.
The relevant types look like this:

```haskell
type GameEvent a = ReaderT Config (WriterT [Text] (EventM Name GameState)) a
type Name = ()
type Scene a = App a () Name

data GameState = GameState
    { _player       :: P.Player
    , _currentLevel :: Int
    , _world        :: World 40 40
    , _history      :: [Text]
    }
```

This implementation is very neat in the sense that we now can talk about *game events*.
A game event could be the player moving, going to the next level, or attacking a monster.
All game events have access to the entire monad stack, so we can:

- add damage modifiers based on the game difficulty
- write game events to the log

For player movement, the process is like this

1. The user presses a key.
2. `appHandleEvent` handles this event (which is now a `BrickEvent`).
3. If the input is one of `WASD`, `handleEvent` takes over, and runs the `moveEvent` function.
   We are now inside the `GameEvent` monad.
4. The move event is handled, possibly triggering other game events.
   All game events (found in `Events.hs`) have the same type, which means that they are very easy to combine
5. The `runEvent` function is used to "collapse" a game event back into an `EventM`.

#### Limitations

> [!NOTE]
> These limitations are also mentioned in `README.md`. Included here for completeness.

There is, however, one limitation that I was not able to work around.
Anything *outside* of the game events cannot access the `Reader` environment, or the log in the `Writer`
One such function is `appDraw`, which naturally needs to know whether the `ASCII-only` flag is enabled.
We work around this by explicitly passing the flag into the function.

Also, the log widget needs to access the "contents" of the `Writer`.
I worked around this by adding the `_history` field to `GameState`. This feels silly, since the whole point of a reader is to *not* pass the log everywhere we go. With this being the ultimate implementation, one could argue that a more pragmatic approach would be to just append directly to the history when a game event occurred. I will not contest this opinion.

The reason for this limitation,  is the types of the fields of `Scene a` (or, well, `App` in the realm of *Brick*).
Here they are in the module `Scenes.Game.Scene`:

```haskell
-- Fields of 'app'. Irrelevant fields left out
appDraw        :: GameState -> [Widget Name]
appHandleEvent :: BrickEvent Name () -> EventM Name GameState ()
```

We observe that the `appDraw` function is beautifully simple. It takes a `GameState`, and spits out the widgets needed to draw it. How elegant!
However, we see that we *cannot* access the monad stack from there.
The only way to do that, would be to change the scene type to `Scene (GameEvent ())`.
Now, *that* complicates *a lot*, and leads to severe refactoring. Furthermore, I do not believe this is the way *Brick* is intended to be used. I did not wish to leave the beaten path without having a very good reason.
For all intents and purposes, I found the eventual implementation to be a reasonable tradeoff.

### GADTs, promoted types, and existential wrappers

This section includes nice usage of the type system that I have sprinkled in different places throughout the source code. Some of these snippets I found *incredibly powerful*, others more fascinating than actually useful.

#### Leveraging GADTs for type safe armour

The player can find and equip armour throughout the game.
Initially, I had one `Armour` type which had a field `_slot`. While this was straightforward to implement, it opened up the possibility of sending the poor protagonist to battle with a pair of boots on his head, and gloves on his feet -- oh no!
This problem *could* be resolved with runtime checks, but come on, we're Haskellers!

The solution was to implement the `Armour` type as a GADT, as following:

```haskell
data Slot = Head | Body | Hands | Feet

data Armour :: Slot -> Type where
    Helmet  :: Material -> Armour 'Head
    Cuirass :: Material -> Armour 'Body
    Gloves  :: Material -> Armour 'Hands
    Boots   :: Material -> Armour 'Feet
```

Now, we can ensure that the correct piece of armour goes the right place on the player -- phew!

```haskell
data Player = Player
	-- ... other fields
    , _helmet  :: Maybe (Armour 'Head)
    , _cuirass :: Maybe (Armour 'Body)
    , _gloves  :: Maybe (Armour 'Hands)
    , _boots   :: Maybe (Armour 'Feet)
    }
```

#### Existential wrappers to the rescue!

While the GADT approach was super handy for dressing the player correctly, another problem arose.
Throughout the game world, chests with items show up. These may contain either a weapon, or a piece of armour.
As the inventory/item system has been stepped down a bit, any item in the game is either a weapon or a piece of armour. Thus, a chest contains `Either Weapon <what goes here??>` -- oh no!
`Armour` is no longer a type, it is parameterised by the slot it goes into!

The solution was wrapping the `Armour` type within `SomeArmour`:

```haskell
data SomeArmour where
    SomeArmour :: Armour s -> SomeArmour
```

This hides the parameter, and all armour pieces may be hidden behind the same wrapper.
This wrapper goes neatly into the `Chest` type.
Initially, I was reluctant for this implementation, as I thought this would result in loss off type information of the armour pieces. However, as we can see in the `equip` function in `Player.hs`, we can pattern match our was back -- and Haskell is even able to check for pattern exhaustiveness!

> [!NOTE] A moment of honesty
> This was an approach which was suggested to me by Bing Copilot (or whatever its name is today).
> It had indeed proposed the same solution to a similar problem earlier, consistently calling it *existential wrappers*. I found no literature on this online, though similar approaches pop up when I google it.

#### Type level dimensions

We turn our heads to `Level.hs`. This module describes the structure of the game world.
Both the `World` and `Level` types (which are tightly related) are parameterised by `cols` and `rows`.
These are `Naturals` that define the dimensions of a `Level`.

This way, we have encoded the dimensions of the level in the *type* of levels!
In turn, this enables us to use the `natVal` function and `Proxy` to query the size of a level (see `Level.hs` for the neat implementation).

> [!NOTE] A moment of realism
>  A realise that this approach does not really provide anything of particular value compared to an implementation that would just query the range of the underlying `Array`. This was the implementation that I used before, which worked just as well.
>  Also, we note that this could in fact be a **limitation**; encoding dimensions into the `Level` type enforces that each level be the same size! This is fine for now, but I can imagine a future where I would like to vary the level size (perhaps even a map that goes beyond the screen!)

### Lazy evaluation at the core

An important feature of this game that I would like to emphasise, is the fact that the world is being randomly generated **on demand**. That means, a level is not created before the player progresses there.
This is powered by Haskell's list, and the `interleaveSequenceIO` function.
Initially, I used `sequence`, but was left with a non-terminating program, as the function evaluates the list before collecting the results. From the [docs](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-Traversable.html#v:sequence):

> [!CITE] Hackage: `sequence`
> Evaluate each monadic action in the structure from left to right, and collect the results...

### Trees that turn into labyrinths

Though not purely a functional programming technique, I am proud of the map generation.
I used *binary space partitioning* to subdivide an initial rectangle into several rooms, and then connected each of them by finding a *minimum spanning tree* between the center of each room.
Take a look as `Tree.hs` for the implementation.

The `BinaryTree` in this module is special in that it only contains data in its leaves.
Furthermore, take note of the `Functor`, `Foldable`, and `Traversable` instances which open the door for beautiful implementations like folding for counting leaves and flattening.

### Property based testing

The implementation of the binary tree leads us nicely into *property based testing*.
Tests can be found in the `Tests.hs` file.
As of now, there are only tests on the `BinaryTree` type -- to demonstrate that I can set up and author property based tests.

The chosen properties to test for are the functor laws, and that the flattening fold behaves as expected.
To be fair, I *could* have dropped the `flatten` function and just opted for `toList`, but we take pointfree folds any day!

## Evaluation of final result

### Which parts of MVP were included in the final result?

We take a look at the status of the MVP:

| Explanation                               | Acceptance criteria                                                                                                              | Status                                                                            |
| ----------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------- |
| Player can choose name and class          | Front page with name input and class selection (*wizard*, *explorer,* etc.)                                                      | ✅                                                                                |
| Procedural random map generation          | Map consists of levels, each level of rooms. Rooms are connected with tunnels, and levels with stairs.                           | ✅                                                                                |
| Player movement, attacks, and using items | Player can move with `WASD`, attack monsters with `R`, and use inventory items by clicking on them.                              | ✅ design changed a little; we use collisions to attack monsters                  |
| Randomly placed chests with items         | Player can find and open chests. Items can be transferred from chest to player inventory.                                        | ✅ scoped down; item system now only contains weapons and armour                  |
| Randomly generated selection of monsters  | At least two monster types that try to kill the player. Implement FSM, movement, attacks, and death.                             | ⚙️ Incomplete; we are missing FSM. Everything else in order                       |
| Player inventory + equipped items         | A separate pane with the player's inventory. The player has armour and weapon slots for equipped items that modify player stats. | ✅ part of the item system being scoped down. equipped armour and weapons visible |
| Player death                              | It's all in the name! HP reaches 0, and the player dies. No saving; you die, you restart.                                        | ✅                                                                                |

This looks good! All of the [milestones](https://github.com/asborv/lambdabyrinth/milestones) but the one about monsters were closed.

### Scoping down items and the inventory

I have mentioned a few times the down-scoping of the inventory system.
There are a few reasons for this:

#### Polymorphic items were difficult!

I spent some time designing how the inventory should look, and how to store items.
Obviously, I would need some kind of common interface (sorry for using the forbidden Java-vocabulary) for items to be stored therein.
Essentially, I wanted an inventory as a list (or something like that), which could store polymorphic `Item`s.
Such items could be weapons, armour, perhaps some potions or scrolls.
However, of the solutions I tried, none of them were completely satisfactory.
Perhaps I could benefit from an existential wrapper like I did with `SomeArmour`, but did not implement it at the time. This solution was suggested by Bing Copilot, but I was reluctant, and eventually dropped the issue

#### More items should be...?

I had already implemented weapons and armour.
Adding more items would mean adding more work for an unknown reward.

## Further improvement

### If only we had more time

- There is still some work to do on the MVP:
	- It bugs me that the monsters do not move
	- Consider taking another shot at a more general item/inventory system
- **Balancing the game**: though there are difficulty options that **do** work, the game is not balanced **at all**. Get yourself a `Diamond Dagger`, and churn through monsters like there's no tomorrow. The game feels very inconsistent and unpredictable.
- **A more pleasant UI**: *Brick* exposes a very elegant API for assigning visual attributes to different parts of the application. We can see this in the `formAttributes` in the `CreateCharacter` module. We could definitely benefit from giving mosters some <span style="color: red;">red</span> background, and perhaps some other cells as well with the `--ascii-only` flag enabled.

### If I were to do it again

Like most other software projects, tests were added as an afterthought.
We didn't cover *QuickCheck* before very late in the semester, which meant I waited until then with adding it.
Ideally, the test suite should be added at the very beginning of the project, ensuring that we were able to access the other modules in the project and actually **run** the tests.

The architecture of the program (the executable template) did not fit the test suite, as it could not load the modules from the executable component.
Fortunately, I was able to refactor it into a library + executable style project, exposing all the game's modules to the test suite. I think these are the reasons i were able to do this in a reasonable amount of time:

- Haskell is an awesome language for refactoring. After understanding what changes to make to the Cabal file, I had no problems moving modules around
- James' **fantastic** walkthrough and [public code](https://github.com/yobson/QuickCheck-Example)!
- [This fantastic video](https://youtu.be/dcEhpPob-wM?si=8OPb8iYs3vXb3bOT&t=304) also showing how to use a library + executable type architecture
- I would consider unifying the application state into **one** type, not three separate, creating a *Brick* application for each. To be fair, this segmented approach suited this application rather well.
