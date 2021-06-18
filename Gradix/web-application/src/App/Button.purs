module App.Button where

import Material
import CSS
import CSS.TextAlign
import CSS.VerticalAlign
import Data.Array
import Data.Either
import Data.Int.Parse
import Data.Maybe
import Data.Show
import Data.Symbol
import Data.Tuple
import Effect.Aff
import Effect.Class
import Effect.Console
import Data.HeytingAlgebra
import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat (ResponseFormat, json)
import Affjax.ResponseFormat as AXR
import App.Dropdown as Dropdown
import Control.Monad.State.Class (get) as H
import Data.Argonaut.Core as JSON
import Data.Argonaut.Decode as AD
import Data.Argonaut.Encode as AE
import Data.Foldable (foldl)
import Data.Int as INT
import Data.Map as MAP
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component (Component, ComponentSlot(..)) as H
import Halogen.HTML (ComponentHTML) as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Core (AttrName(..), HTML(..)) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA
import Prelude (Unit, bind, const, discard, pure, unit,negate,(>=), (-),(/=), (==), ($), (*), (+), (<$>), (<<<), (<>))
import Select as S
import Internal.CSS (class_, classes_, whenElem)
import Web.XHR.XMLHttpRequest (response)
type Student  = {_sId :: Int, _sName::String, _sGroup :: String ,_sCode::String}
type Attendance = {_aId :: Int, _aStudentId ::Int, _aSeminar :: Int, _aGroup :: String, _aActivity :: Boolean}
type ParentState = {inputString :: String, plugin :: String, _aGroup::String, _aSeminar ::Int}
_gridComponent = SProxy :: SProxy "gridComponent"
type ParentSlots = (gridComponent :: forall output query. H.Slot query output Int, dropdown::Dropdown.Slot)
data ParentAction = InitializeParent | FillInput String | PluginHandle Dropdown.Message | GroupHandle Dropdown.Message| SeminarHandle Dropdown.Message
parentComponent :: forall q i o. H.Component HH.HTML q i o Aff
parentComponent  = H.mkComponent{
  initialState:const {inputString : "", plugin: "Attendance", _aGroup :"931", _aSeminar:1},
  render,
  eval: H.mkEval $ H.defaultEval {handleAction = handleParent, initialize = Just InitializeParent}
  }where 
    render :: ParentState ->  H.ComponentHTML ParentAction ParentSlots Aff
    render state = HH.div_[ 
      HH.label 
      [classes_ ["mdc-text-field","mdc-text-field--filled","space"]] 
        [HH.span 
          [class_ "mdc-text-field__ripple"]
          []

        , HH.span 
            [ class_ "mdc-floating-label"
            , HP.id_ "my-label-id" ]
            [ HH.text "Student Name" ]
        , HH.input 
            [ class_ "mdc-text-field__input"
            , HA.labelledBy "my_label_id"
            , HP.type_ HP.InputText
            , HE.onValueInput (Just <<< FillInput)]
        , HH.span 
          [classes_ ["mdc-line-ripple"]]
          []
        ] 
        , HH.slot label 0 Dropdown.component input1 (Just <<< PluginHandle)
        , HH.slot label 1 Dropdown.component input2 (Just <<< GroupHandle)
        , HH.slot label 2 Dropdown.component input3 (Just <<< SeminarHandle),
      HH.slot _gridComponent 0 gridComponent state (\_->Nothing)
      ] where
        label = SProxy :: SProxy "dropdown"
        input1 = { items: [ "Attendance", "Activity"],selection : Just "Attendance", buttonLabel: "settings_applications|Plugin" }
        input2 = {items : ["931","932","933","934","935","936","937"], selection: Just "931", buttonLabel: "group|Group"}
        input3 = {items : ["1","2","3","4","5","6","7","8","9","10","11","12","13","14"],selection: Just "1", buttonLabel : "format_list_numbered|Seminar"}
    handleParent ::forall o . ParentAction -> H.HalogenM ParentState ParentAction ParentSlots o Aff Unit
    handleParent = case _ of
      InitializeParent -> pure upgradeElements
      PluginHandle (Dropdown.SelectionChanged x y) -> H.modify_ _{plugin = maybe "" (\a->a) y}
      GroupHandle (Dropdown.SelectionChanged x y)  -> H.modify_ _ {_aGroup = maybe "" (\a-> a) y}
      SeminarHandle (Dropdown.SelectionChanged x y ) -> H.modify_ _{_aSeminar = maybe 0 (\a->maybe 0 (\b->b) (parseInt a(toRadix 10))) y }
      FillInput x -> H.modify_ _{inputString = x}


jsonToStudents :: JSON.Json -> Either String (Array Student)
jsonToStudents = AD.decodeJson

jsonToAttendances :: JSON.Json -> Either String (Array Attendance)
jsonToAttendances = AD.decodeJson

attendanceToJson :: Attendance -> JSON.Json
attendanceToJson =AE.encodeJson

type GridState = {students :: Array Student, columns :: Int, attendanceMap :: MAP.Map Int (Tuple Boolean Boolean), _aSeminar ::Int, _aGroup::String, plugin::String, inputString :: String, hoverId :: Int}
data GridAction = Receive GridInput | Click Int | Initialize | Hover (Maybe Student)


getNameClass :: GridState -> Int -> Int -> String
getNameClass state i j = 
        case (index state.students (i*state.columns+j)) of
            Nothing -> ""
            Just stud -> case MAP.lookup stud._sId state.attendanceMap of
                             Nothing -> ""
                             Just (Tuple attendance activity) -> case attendance of
                                                                      false -> "absence"
                                                                      true -> case activity of
                                                                                  false -> "attendance"
                                                                                  true -> "activity"

type GridInput = {inputString :: String,_aSeminar::Int, _aGroup ::String, plugin::String}
gridComponent :: forall q o. H.Component HH.HTML q GridInput o Aff
gridComponent = H.mkComponent {
  initialState,
  render ,
  eval : H.mkEval $ H.defaultEval 
      {handleAction = gridHandle,
      initialize = Just Initialize
      , receive = Just <<< Receive
      }
} where
    initialState :: GridInput -> GridState
    initialState {inputString:is,_aSeminar:s,_aGroup:g,plugin:p} = {students : [], columns :5, attendanceMap : MAP.empty, _aSeminar:s,_aGroup:g,plugin:p,inputString:is, hoverId:(-1)} 
    render :: GridState -> H.ComponentHTML GridAction () Aff
    render state = 
      HH.div_
      [ case length state.students of
            0-> HH.text ""
            _ -> HH.ul [class_ "mdc-list"] $ row <$> range 1 rows] 
      where
        studNr = length state.students
        reminder = INT.rem studNr state.columns
        divisionResult = INT.quot studNr state.columns
        rows = case reminder of
                      0 -> divisionResult
                      _ -> divisionResult + 1
        row i = HH.ul [class_ "mdc-list"] $ cell i <$> range 1 ( case (i == rows) && (reminder /=0) of 
                                                  true -> reminder
                                                  false -> state.columns)
        cell i j = HH.span
                  [classes_ ["mdc-card", "my-card"]
                  , HE.onClick (\_ -> Just (Click (maybe (-1) (\a->a._sId) stud)))
                  , HE.onMouseOver (\_ -> Just (Hover stud))
                  , HE.onMouseLeave (\_ -> Just (Hover Nothing))]
                  [
                    HH.button [classes_ ["mdc-button","mdc-card__action","mdc-card__action--button","nameBox"]]
                    [
                    HH.i [ classes_ ["material-icons","mdc-button__icon"]
                          , HA.hidden "true"]
                          [HH.text $ case (getNameClass state (i-1) (j-1)) of
                                   "activity" -> "sentiment_very_satisfied"
                                   "attendance" -> "sentiment_satisfied"
                                   _ -> "sentiment_very_dissatisfied"]
                  , HH.span [class_ "mdc-button__label"]
                           [HH.text $ case stud of 
                              Just x ->x._sName
                              Nothing -> ""]]
                  ,case stud of
                       Nothing -> HH.text ""
                       Just student -> HH.ul [classes_ ["mdc-list","my-card-hover"]] 
                                      [ hoverItem ("Group : "<>student._sGroup) 
                                      , hoverItem ("Code: "<>student._sCode)]
                  ]
                  where 
                    stud = index state.students ((i-1)*state.columns+j-1)

hoverItem ::forall w i .String -> HH.HTML w i
hoverItem label = 
       HH.li[classes_ ["mdc-list-item","list-item"]]
          [ HH.span [class_ "mdc-list-item__text"] [HH.text label]]
gridHandle :: forall o. GridAction -> H.HalogenM GridState GridAction () o Aff Unit
gridHandle = case _ of
  Initialize -> do
     group <- H.gets _._aGroup
     seminar <- H.gets _._aSeminar
     inputString <- H.gets _.inputString
     seminarData <- H.liftAff $ AX.get json ("http://localhost:8081/attendances/"<>show seminar<>"/"<>group)
     studentsData <- case inputString of
                           "" -> H.liftAff $ AX.get json ("http://localhost:8081/students/sort/" <> show seminar <> "/" <> group)
                           _  -> H.liftAff $ AX.get json ("http://localhost:8081/students/filter/" <> inputString)
     let attendances = case seminarData of
             Right response -> let eitherResult = (jsonToAttendances response.body) in
               case eitherResult of
                 Right attds -> attds
                 Left _ -> []
             Left _ -> []
         studs =  case studentsData of
             Right response -> case (jsonToStudents response.body) of
                 Right studs -> studs
                 Left _ -> []
             Left _ -> []
     H.modify_ _{students = studs}
     H.modify_ _{attendanceMap = (foldl (\map student -> MAP.insert student._sId (getTuple student._sId attendances) map) MAP.empty studs)} 
  Receive {_aGroup:gr, _aSeminar:sem, inputString:istr, plugin : plg} -> do 
      seminarData <- H.liftAff $ AX.get json ("http://localhost:8081/attendances/"<>show sem<>"/"<>gr)
      studentsData <- case istr of
                      "" -> H.liftAff $ AX.get json ("http://localhost:8081/students/sort/"<>show sem<>"/"<>gr)
                      _  -> H.liftAff $ AX.get json ("http://localhost:8081/students/filter/" <> istr)
      let attendances = case seminarData of
            Right response -> let eitherResult = (jsonToAttendances response.body) in
              case eitherResult of
                Right attds -> attds
                Left _ -> []
            Left _ -> []
      let studs =  case studentsData of
            Right response -> case (jsonToStudents response.body) of
                Right studs -> studs
                Left _ -> []
            Left _ -> []
      H.modify_ _{students = studs, _aSeminar=sem, _aGroup = gr, inputString= istr, plugin=plg} 
      H.modify_ _{attendanceMap = (foldl (\map student -> MAP.insert student._sId (getTuple student._sId attendances) map) MAP.empty studs)}
  Click id -> do
      attendances <- H.gets _.attendanceMap
      plugin <- H.gets _.plugin
      seminar <- H.gets _._aSeminar
      group <- H.gets _._aGroup
    
      let tuple = (case plugin of
            "Activity" -> case (MAP.lookup id attendances) of
              Nothing -> Tuple true true
              Just tup -> Tuple true (not(snd tup))
            _  -> case (MAP.lookup id attendances) of
              Nothing -> Tuple true false
              Just tup -> Tuple (not (fst tup)) false)
      liftEffect $ log (show tuple)
      H.modify_ _{attendanceMap = MAP.insert id tuple attendances}
      result <- case plugin of
          "Attendance" ->H.liftAff$ AX.post_ ("http://localhost:8081/attendance") (Just $ AXRB.json (AE.encodeJson {_aId:0,_aStudentId:id,_aSeminar:seminar,_aGroup:group,_aActivity: (snd tuple)}))
          _ -> H.liftAff$ AX.post_ ("http://localhost:8081/activity") (Just $AXRB.json(AE.encodeJson {_aId:0,_aStudentId:id,_aSeminar:seminar,_aGroup:group,_aActivity: (snd tuple)}))
      pure unit
  Hover mst -> do
    case mst of
        Nothing -> H.modify_ _{hoverId = (-1)}
        Just st -> H.modify_ _{hoverId = st._sId}
getTuple :: Int -> Array Attendance -> Tuple Boolean Boolean
getTuple id attendances = case (findIndex (\at-> id == at._aStudentId) attendances) of
                                           Nothing -> Tuple false false
                                           Just i -> case (index attendances i) of
                                                         Nothing -> Tuple false false
                                                         Just at -> Tuple true at._aActivity
