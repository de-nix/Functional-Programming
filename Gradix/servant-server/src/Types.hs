{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Types where

import           Control.Concurrent.MonadIO
import           Control.Concurrent.STM
import           Control.Error.Util
import qualified Control.Lens
import           Control.Lens.TH
import           Control.Monad.Catch
import           Data.Aeson
import qualified Data.Map                     as Map
import           Data.Time
import           GHC.Generics

type StudentName   = String
type StudentId     = Integer
type Group         = String
type Code          = String
type AttendanceId  = Integer
type SeminarNumber = Integer

type LocalData = TVar (Map.Map StudentName (Maybe Code, Maybe SeminarNumber, Maybe Group))

data Info = Info 
  { iid :: String
  , name :: String
  }deriving(Generic,Eq,Show)

data Conversation = Conversation
 {conversationId :: String
 ,tenantId :: String
 ,conversationType :: String} deriving(Show)                                             

data Message = Message
  { mid :: String
  , text :: String
  , timestamp :: UTCTime
  , from :: Info
  , conversation :: Conversation
  , recipient :: Info
  }deriving(Generic,Show)

data Response = Response
  {textResponse :: String
  ,typeResponse ::String
  ,fromResponse ::Info
  ,recipientResponse:: Info
  ,conversationResponse :: Conversation
  ,replyToId :: String}deriving(Show)

data Token = Token
  { access_token :: String} deriving(Generic,Show)

instance FromJSON Token where
  parseJSON (Object x) = Token <$> x.: "access_token"
  parseJSON _ = fail "no obj"

instance ToJSON Info where
  toJSON i = object ["id" .= iid i
                    ,"name" .= name i]

instance FromJSON Info where
  parseJSON (Object x) = Info <$> x.: "id" <*> x.: "name"
  parseJSON _ = fail "expected an object in info"

instance FromJSON Conversation where
  parseJSON (Object x) = Conversation <$> x.: "id"<*> x.: "tenantId" <*> x.: "conversationType"
  parseJSON _ = fail "expected an obj "

instance ToJSON Conversation where
  toJSON c = object ["id" .= conversationId c
                    ,"tenantId" .= tenantId c
                    ,"conversationType" .= conversationType c]

instance ToJSON Response where
  toJSON r = object ["text" .= textResponse r
                    ,"type" .= typeResponse r
                    ,"from" .= fromResponse r
                    ,"recipient" .= recipientResponse r
                    ,"conversation" .= conversationResponse r
                    ,"replyToId" .= replyToId r]

instance FromJSON Message where
  parseJSON (Object message) = Message <$> message.: "id" <*> message.: "text"
    <*> message.: "timestamp" <*> message .: "from" <*> message .: "conversation"
    <*> message.: "recipient"
      

instance ToJSON Message where
  toJSON r = object [ "id" .= mid r
                    , "text" .= text r
                    , "timestamp" .= timestamp r
                    , "from" .= Types.from r
                    , "conversation" .= conversation r
                    , "recipient" .= recipient r]

data Student = Student
    { _sId    :: StudentId
    , _sName  :: String
    , _sGroup :: Group
    , _sCode  :: Code
    } deriving (Generic, Show)

instance Eq Student where
  student1 == student2 = (_sId student1 == _sId student2) 
                         || (_sCode student1 == _sCode student2)

$(makeLenses ''Student)

data Attendance = Attendance 
    { _aId        :: AttendanceId
    , _aStudentId :: StudentId
    , _aSeminar   :: SeminarNumber
    , _aGroup     :: Group
    , _aActivity  :: Bool
    } deriving (Generic, Show)

instance Eq Attendance where
  attendance1 == attendance2 =  (_aId attendance1 == _aId attendance2) || 
                                ((_aSeminar attendance1 == _aSeminar attendance2 ) 
                                && (_aStudentId attendance1 == _aStudentId attendance2)) 


$(makeLenses ''Attendance)

instance ToJSON Student where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Student

instance ToJSON Attendance where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Attendance

class ModelAPI a m where
    createStudent    :: MonadIO m => Student      -> a -> m ()
    removeStudent    :: MonadIO m => StudentId    -> a -> m ()
    updateStudent    :: MonadIO m => Student      -> a -> m ()
    findStudent      :: MonadIO m => StudentId    -> a -> m (Maybe Student)
    getAllStudents   :: MonadIO m =>                 a -> m ([Student])
    createAttendance :: MonadIO m => Attendance   -> a -> m ()
    removeAttendance :: MonadIO m => AttendanceId -> a -> m ()
    updateAttendance :: MonadIO m => Attendance   -> a -> m ()
    findAttendance   :: MonadIO m => AttendanceId -> a -> m (Maybe Attendance)
    getAllAttendances:: MonadIO m =>                 a -> m ([Attendance])
