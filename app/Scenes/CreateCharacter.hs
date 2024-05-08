{- |
Module      :  Scenes.CreateCharacter
Description :  Scene for character creation form.
               The code is based on the Brick form demo: https://github.com/jtdaugherty/brick/blob/master/programs/FormDemo.hs#L45
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Scenes.CreateCharacter (createCharacter) where

import Brick
    ( App (..)
    , BrickEvent (VtyEvent)
    , Widget
    , attrMap
    , defaultMain
    , halt
    )
import Brick.AttrMap (AttrMap)
import Brick.Focus (focusRingCursor)
import Brick.Forms
import Brick.Types (EventM)
import Brick.Util (on)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core
import Creatures.Player
import Graphics.Vty

-- | Field names for the player creation form
data Name
    = NameField
    | WizardField
    | RogueField
    | WarriorField
    deriving (Eq, Ord, Show)

-- | Create a form given a player
mkForm :: Player -> Form Player e Name
mkForm =
    let label s w = padBottom (Pad 1) $ vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
     in newForm
            [ label "Name" @@= editTextField name NameField (Just 1)
            , label "Class"
                @@= radioField
                    characterClass
                    [ (Wizard, WizardField, "Wizard")
                    , (Rogue, RogueField, "Rogue")
                    , (Warrior, WarriorField, "Warrior")
                    ]
            ]

formAttributes :: AttrMap
formAttributes =
    attrMap
        defAttr
        [ (invalidFormInputAttr, white `on` red)
        , (focusedFormInputAttr, black `on` yellow)
        ]

handleEvent :: BrickEvent Name e -> EventM Name (Form Player e Name) ()
handleEvent (VtyEvent (EvKey KEnter [])) = halt
handleEvent e = handleFormEvent e

drawScene :: Form Player e Name -> [Widget Name]
drawScene f = [center . borderWithLabel (txt "Create character") $ renderForm f]

app :: App (Form Player e Name) e Name
app =
    App
        { appDraw = drawScene
        , appChooseCursor = focusRingCursor formFocus
        , appHandleEvent = handleEvent
        , appStartEvent = return ()
        , appAttrMap = const formAttributes
        }

{- | Display a scene to allow character creation.
When the character has been created, the user can press 'Enter' to continue with this character.
-}
createCharacter :: IO Player
createCharacter = formState <$> defaultMain app (mkForm initialPlayer)
  where
    initialPlayer =
        Player
            { _name = ""
            , _pos = (0, 0)
            , _hand = Nothing
            , _helmet = Nothing
            , _cuirass = Nothing
            , _gloves = Nothing
            , _boots = Nothing
            , _health = 200
            , _characterClass = Wizard
            }
