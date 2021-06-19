module App.Dropdown where

import Prelude
import Material
import Data.String
import Data.Array (head,last,(!!), mapWithIndex, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.ARIA as HA
import Internal.CSS (class_, classes_, whenElem)
import Select as S
import Select.Setters as SS


data Query a
  = Reply (Unit -> a)
  | Command a
type Slot = 
        S.Slot Query () Message Int

data Action  = Initialize
type State =
  ( items :: Array String
  , selection :: Maybe String
  , buttonLabel :: String
  )

data Message
  = SelectionChanged (Maybe String) (Maybe String)

-- it is unnecessary to export your own input type, but doing so helps if you
-- would like to set some sensible defaults behind the scenes.
type Input =
  { items :: Array String
  , selection :: Maybe String
  , buttonLabel :: String

  }

component :: S.Component Query () Input Message Aff
component = S.component input $ S.defaultSpec
  { render = render
  , handleEvent = handleEvent
  , handleAction = handleAction
  , initialize = Just Initialize
  }
  where
  input :: Input -> S.Input State
  input { items,selection, buttonLabel } =
    { inputType: S.Toggle
    , search: Nothing
    , debounceTime: Nothing
    , getItemCount: length <<< _.items
    , items
    , buttonLabel
    , selection
    }

  handleEvent :: S.Event -> S.HalogenM State Action () Message Aff Unit
  handleEvent = case _ of
    S.Selected ix -> do
      st <- H.get
      let selection = st.items !! ix
      H.modify_ _ { selection = selection, visibility = S.Off }
      H.raise $ SelectionChanged st.selection selection
    S.VisibilityChanged S.On -> do
      pure $ upgradeElements 4
    _ -> pure unit

  render ::S.State State -> S.ComponentHTML Action () Aff
  render st =
    HH.div
    [ classes_ ["inline","mdc-menu-surface__anchor"] ]
      [ renderToggle, renderContainer ]
    where
    renderToggle =
      HH.button
        ( SS.setToggleProps [ classes_ ["mdc-button","mdc-button--raised"]] )
        [ HH.span [class_ "mdc-button__ripple"]
                  []
        , HH.i [classes_ ["material-icons","mdc-button__icon"], HA.hidden "true"]
            [HH.text 
            $ fromMaybe "" $ head $ split (Pattern "|") st.buttonLabel
                         ]
        , HH.span [class_ "mdc-button__label"] 
            [HH.text (fromMaybe (fromMaybe "" $ last $ split (Pattern "|") st.buttonLabel) st.selection)]
        ]
    renderContainer = whenElem (st.visibility == S.On) \_ ->
      HH.div [classes_ ["mdc-menu", "mdc-menu-surface"]]
      [HH.ul
        ( SS.setContainerProps [ class_ "mdc-list" ] )
        ( renderItem `mapWithIndex` st.items )
      ]
      where
      renderItem index item =
        HH.li
          ( SS.setItemProps index
              [ classes_
                  [ "mdc-list-item" # guard (st.highlightedIndex == Just index)]
              ]
          )
          [ HH.span[class_ "mdc-list-item__ripple"][]
          , HH.span [class_ "mdc-list-item__text"][HH.text item ]
          ]
  handleAction ::Action -> S.HalogenM State Action () Message Aff Unit
  handleAction Initialize = pure $ upgradeElements 4
