I am making a game in a course in advanced functional programming.
We use haskell, and are supposed to demonstrate competency in advanced FP concepts.
I will present the structure of my project to you, and all of its code. Then, I will ask you some questions.

Here is the program structure:

```
├── CHANGELOG.md
├── LICENSE
├── Project proposal.pdf
├── README.md
├── app
│   ├── Creatures
│   │   ├── Creature.hs
│   │   ├── Monsters.hs
│   │   └── Player.hs
│   ├── Items.hs
│   ├── Main.hs
│   └── World.hs
├── dist-newstyle
│   ├── build
│   └── ...other irrelevant compiler files
├── ghc-9.6.4.hp
├── lambdabyrinth.cabal
└── src
    └── creatures```

Here is all the code:

```haskell
{- |
Module      : Items
Description : Items and friends
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items where

data ArmourPiece = Helmet | Cuirass | Gloves | Boots deriving (Show)
data WeaponType = Sword | Spear deriving (Show)
data Material = Stone | Iron | Diamond deriving (Show)

data Item
    = Armour Material ArmourPiece
    | Weapon Material WeaponType

instance Show Item where
    show (Armour material piece) = show material <> " " <> show piece
    show (Weapon material weaponType) = show material <> " " <> show weaponType

power :: Item -> Int
power (Weapon material weaponType) = materialBonus material * weaponBonus
  where
    weaponBonus = case weaponType of
        Sword -> 5
        Spear -> 3
power _ = 0

materialBonus :: Material -> Int
materialBonus = \case
    Stone -> 5
    Iron -> 15
    Diamond -> 30

armorPieceBonus :: ArmourPiece -> Int
armorPieceBonus = \case
    Helmet -> 12
    Cuirass -> 24
    Gloves -> 8
    Boots -> 10

armourDefence :: Item -> Int
armourDefence (Armour material piece) = materialBonus material * armorPieceBonus piece
armourDefence _ = 0
module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Lens (to, (%~), (^.))
import Control.Lens.Lens ((&))
import Control.Lens.TH
import Creatures.Player
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Arr
import Graphics.Vty
import Items
import World

type Name = ()

data GameState = GameState
    { _player :: Player
    , _currentLevel :: Int
    , _world :: World
    }
    deriving (Show)

makeLenses ''GameState

app :: App GameState () Name
app =
    App
        { appDraw = drawGame
        , appChooseCursor = neverShowCursor
        , appHandleEvent = \case
            VtyEvent e -> case e of
                EvKey (KChar 'q') [] -> halt
                -- Movement
                EvKey (KChar 'w') [] -> modify (player . pos %~ \(y, x) -> (y - 1, x))
                EvKey (KChar 'a') [] -> modify (player . pos %~ \(y, x) -> (y, x - 1))
                EvKey (KChar 's') [] -> modify (player . pos %~ \(y, x) -> (y + 1, x))
                EvKey (KChar 'd') [] -> modify (player . pos %~ \(y, x) -> (y, x + 1))
                EvKey (KChar 'b') [] -> modify (currentLevel %~ (+ 1))
                EvKey (KChar 'B') [] -> modify (currentLevel %~ subtract 1)
                EvKey (KChar 'i') [] -> modify (player .inventory %~ (Weapon Diamond Spear:))
                _ -> return ()
            _ -> return ()
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap defAttr []
        }

drawGame :: GameState -> [Widget Name]
drawGame game =
    let ui = drawLevel game <=> drawInventory game
     in [ui]

drawInventory :: GameState -> Widget Name
drawInventory game =
    borderWithLabel
        (txt "Inventory")
        (center $ vBox $ map (txt . T.pack . show) (game ^. player . inventory))

drawLevel :: GameState -> Widget Name
drawLevel game = borderWithLabel (txt "Lambdabyrinth") $ center $ vBox (hBox <$> rows)
  where
    level = (game ^. world) !! (game ^. currentLevel)
    rows = chunksOf (width level) $ do
        (coord, cell) <- level ^. cells & assocs
        let monster = level ^. monsters . to (Map.lookup coord)
        return $
            if (game ^. player . pos) == coord
                then txt $ T.pack $ show $ game ^. player
                else txt $ maybe (T.pack $ show cell) (T.pack . show) monster

main :: IO ()
main = do
    let initialState = GameState mrBean 0 [emptyLevel, firstLevel]
    finalState <- defaultMain app initialState
    print finalState

mrBean :: Player
mrBean =
    Player
        { _name = "Mr. Bean"
        , _pos = (0, 0)
        , _hand = Nothing
        , _helmet = Nothing
        , _cuirass = Nothing
        , _gloves = Nothing
        , _boots = Nothing
        , _inventory = [Armour Iron Helmet, Weapon Stone Sword]
        , _health = 10
        , _characterClass = Wizard
        }
{- |
Module      : World
Description : Building blocks for the game world
Maintainer  : asbjorn.orvedal@gmail.com
-}
module World where

import Control.Arrow ((>>>))
import Control.Lens (both, makeLenses, over, to, (^.))
import qualified Data.Map as Map
import GHC.Arr
import Creatures.Monsters

type Coordinate = (Int, Int)
type World = [Level]

data VerticalDirection = Upwards | Downwards
data Cell
    = Door
    | Empty
    | Floor
    | Stair VerticalDirection
    | Tunnel
    | Wall

instance Show Cell where
    show (Stair Downwards) = "V "
    show (Stair Upwards) = "Λ "
    show Door = "λ "
    show Empty = "ε "
    show Floor = ". "
    show Tunnel = "| "
    show Wall = "# "

data Level = Level
    { _cells :: Array Coordinate Cell
    , _monsters :: Map.Map Coordinate Monster
    }
    deriving (Show)

makeLenses ''Level

-- | Get the width and height of the level
dimensions :: Level -> (Int, Int)
dimensions level = level ^. cells . to (bounds >>> snd >>> over both (+ 1))

-- | Get only the width of the level
width :: Level -> Int
width = snd . dimensions

-- | Get only the height of the level
height :: Level -> Int
height = fst . dimensions

-- ===============
-- Constant levels
-- ===============

emptyLevel :: Level
emptyLevel = Level (listArray ((0, 0), (9, 9)) (repeat Floor)) Map.empty

firstLevel :: Level
firstLevel = Level cs ms
  where
    cs = listArray
        ((0, 0), (12, 9))
        [ Empty, Empty, Empty, Empty, Empty,  Empty, Empty, Empty, Empty, Empty
        , Empty, Wall, Wall,   Wall,  Wall,   Wall,  Wall,  Wall,  Empty, Empty
        , Empty, Door, Floor,  Floor, Floor,  Floor, Floor, Wall,  Empty, Empty
        , Empty, Wall, Floor,  Floor, Floor,  Floor, Floor, Wall,  Empty, Empty
        , Empty, Wall, Wall,   Wall,  Door,   Wall,  Wall,  Wall,  Empty, Empty
        , Empty, Empty, Empty, Empty, Tunnel, Empty, Empty, Empty, Empty, Empty
        , Empty, Empty, Empty, Empty, Tunnel, Empty, Empty, Empty, Empty, Empty
        , Empty, Empty, Empty, Empty, Tunnel, Empty, Empty, Empty, Empty, Empty
        , Empty, Empty, Wall,  Wall,  Door,   Wall,  Wall,  Wall,  Empty, Empty
        , Empty, Empty, Wall,  Floor, Floor,  Floor, Floor, Wall,  Empty, Empty
        , Empty, Empty, Wall,  Floor, Floor,  Floor, Floor, Wall,  Empty, Empty
        , Empty, Empty, Wall,  Wall,  Wall,   Door,  Wall,  Wall,  Empty, Empty
        , Empty, Empty, Empty, Empty, Empty,  Empty, Empty, Empty, Empty, Empty
        ]
    ms = Map.fromList [((3, 2), Zombie)]
{- |
Module      : Monsters
Description : Definition of all the monsters in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Creatures.Monsters where

import Creatures.Creature

data Monster = Zombie | Ghost

instance Show Monster where
    show Zombie = "🧟\b "
    show Ghost = "👻\b "

instance Creature Monster where
    attackPower :: Monster -> Int
    attackPower = \case
        Zombie -> 15
        Ghost -> 8

    defence :: Monster -> Int
    defence = \case
        Zombie -> 32
        Ghost -> 4
{- |
Module      : Player
Description : Everything that has to do with the player
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Creatures.Player where

import Control.Lens
import Creatures.Creature
import Items
import World (Coordinate)

data Class = Wizard | Warrior | Rogue

data Player = Player
    { _name :: String
    , _pos :: Coordinate
    , _hand :: Maybe Item
    , _helmet :: Maybe Item
    , _cuirass :: Maybe Item
    , _gloves :: Maybe Item
    , _boots :: Maybe Item
    , _inventory :: [Item]
    , _health :: Int
    , _characterClass :: Class
    }

makeLenses ''Player

instance Show Player where
    show _ = "😎\b "

instance Creature Player where
    attackPower :: Player -> Int
    attackPower player = (player ^. characterClass & classPower) + weaponPower
      where
        weaponPower = player ^. hand & maybe 0 power

    defence :: Player -> Int
    defence player = classDefence + armourBonus
      where
        -- Intermediate values
        helmetBonus  = player ^. helmet & maybe 0 armourDefence
        cuirassBonus = player ^. cuirass & maybe 0 armourDefence
        gloveBonus   = player ^. gloves & maybe 0 armourDefence
        bootBonus    = player ^. boots & maybe 0 armourDefence
        -- Values actually used in computation
        armourBonus  = helmetBonus + cuirassBonus + gloveBonus + bootBonus
        classDefence = player ^. characterClass & classPower

classPower :: Class -> Int
classPower = \case
    Wizard -> 15
    Warrior -> 85
    Rogue -> 45
module Creatures.Creature where

class Creature a where
    attackPower :: a -> Int
    defence :: a -> Int
```

The files are split by module; I trust you can figure out which belong to which files.
Now, for my question:

As you can see, I am making an inventory system with items.
`Item` is an ADT which separates into different kinds of items.
The neat thing with this, is that the `Inventory` implementation is super-duper easy for the player!
However, this is at the expense of the type system. As you can see, I want to be able to calculate the defence points of any piece of armour. However, in the current implementation, it is legal to pass a weapon or potion to the defence calculator, that makes no sense! Continuing down the road of nonsense, it is also possible to equip a sword as a helmet, or health potion as boots. Again, completely nonsesical.

The crux of the problem is:
How do we build a type system such that the inventory can hold any kind of item (but no other types), still retainint type safety?
How can we make sure that the player's armour and weapon slots only hold intended kinds of items? We wish to be able to pick an item form the inventory, and put it on a player's head if it is a helmet; so the inventory *must* retain type information. We cannot rely on runtype checks. 

## Some proposed solutions

### Using GADTs

I've seen GADTs being used for similar problems; we are able to parameterize an ADT, and have concrete types for the different GADT constructors.
This is super handy for creating a type safe arithmetic/boolean expression interpreter.
I thought to include one here, with an implementation something like the following:

```haskell
data ItemKind = ArmourKind | WeaponKind -- | other types
data Item a where
    Armour :: Material -> ArmourPiece -> Item ArmourKind
    Weapon :: Material -> WeaponKind -> Item WeaponKind
```

This makes it able to give `Item ArmourType` as a type signature to the defence calculator, and the same for the attack calculation of weapons.
However, this complicates the inventory type, as I cannot make out which type I shall give to the inventory field of the `Player` constructor:

```haskell
data Player = Player
    { _name :: String
    , _pos :: Coordinate
    , _hand :: Maybe (Item WeaponType)
    , _helmet :: Maybe (Item ArmourType)
    , _cuirass :: Maybe (Item ArmourType)
    , _gloves :: Maybe (Item ArmourType)
    , _boots :: Maybe (Item ArmourType)
    , _inventory :: -- what goes here? [Item <something>]
    , _health :: Int
    , _characterClass :: Class
    }
```

- It cannot just be `Item`, since that has the wrong kind
- `Item (Armour|Weapon)Kind` works, but that defeats the polymorphic nature of the inventory
- I also tried something like `forall a. ItemType a => [Item a]`, but that also gives an error on the kind

Is GADTs a good approach here? I also considered type classes to mimic interfaces/abstract classes I would have used in an OOP approach to this problem. Feel free to invalidate my solution if you believe it best, but then you must come up with an alternative, and explain why it is better.

Can we make this work using GADTs?