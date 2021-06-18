{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators     #-}

module ServantModule where

import           Control.Concurrent.MonadIO
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar, readTVarIO)
import           Control.Error.Util
import           Control.Monad.Catch
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Data.Aeson                  (decode, encode, object, (.=))
import           Data.ByteString.Internal
import           Data.Char
import           Data.Function               (on)
import           Data.List                   
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Pool                   (Pool, withResource)
import           Data.Proxy
import           Data.Time
import           JSON
import           Network.HTTP.Client         (responseBody, RequestBody(..), httpLbs
                                             , urlEncodedBody, parseRequest, newManager
                                             , requestBody, Request(..), method)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Simple         (addRequestHeader)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant.API
import           Servant.Server
import           SQL
import           Types

runServer :: AppConfig SQLConnection -> IO()
runServer config = run 8081 $ app config

app config = cors (const $ Just policy) $ serve api $ hoistServer api (nt config) server
  where
  policy = simpleCorsResourcePolicy { 
      corsRequestHeaders = ["Content-Type"], 
      corsMethods ="DELETE" : ("GET":("PUT":( "POST" : simpleMethods)))
      }
  nt config reader = liftIO $ runReaderT reader config

api :: Proxy API
api = Proxy

server = findStudHandler   :<|> addStudHandler   :<|> updateStudHandler
    :<|> removeStudHandler :<|> sortStudsHandler :<|> filterStudsHandler 
    :<|> findAttHandler    :<|> switchAttHandler :<|> updateAttHandler
    :<|> removeAttHandler  :<|> getAttsHandler   :<|> switchActHandler  
    :<|> teamsAttHandler

type App a m v = ReaderT (AppConfig a) m v
type API = "student"     :> Capture "id" Integer :> Get '[JSON] (Maybe Student)
      :<|> "student"     :> ReqBody '[JSON] Student :> Post '[JSON] ()
      :<|> "student"     :> ReqBody '[JSON] Student :> Put '[JSON] ()
      :<|> "student"     :> Capture "id" Integer :> Delete '[JSON] ()
      :<|> "students"    :>"sort"  :> Capture "seminar" Integer 
                         :> Capture "group" String :> Get '[JSON] [Student]
      :<|> "students"    :>"filter":> Capture "name" String :> Get '[JSON] [Student]
      :<|> "attendance"  :> Capture "id" Integer :> Get '[JSON] (Maybe Attendance)
      :<|> "attendance"  :> ReqBody '[JSON] Attendance :> Post '[JSON] ()
      :<|> "attendance"  :> ReqBody '[JSON] Attendance :> Put '[JSON] ()
      :<|> "attendance"  :> Capture "id" Integer :> Delete '[JSON] ()
      :<|> "attendances" :> Capture "seminar" Integer :> Capture "group" String 
                         :> Get '[JSON] [Attendance]
      :<|> "activity"    :> ReqBody '[JSON] Attendance :> Post '[JSON] ()
      :<|> "message"     :> ReqBody '[JSON] Message :> Post '[JSON] ()

data AppConfig a = AppConfig
  { connection :: a
  , state      :: TVar (Map.Map StudentName (Maybe Attendance))
  }
  
addStudHandler ::(ModelAPI a m, MonadIO m) => Student -> App a m ()
addStudHandler student = do
  config <- ask
  lift $ createStudent student (connection config)

findStudHandler :: (ModelAPI a m, MonadIO m) => Integer -> App a m (Maybe Student)
findStudHandler id = do
  config <- ask
  lift $ findStudent id (connection config)

removeStudHandler ::(ModelAPI a m, MonadIO m) => Integer -> App a m ()
removeStudHandler id = do
  config <- ask
  lift $ removeStudent id (connection config)

updateStudHandler ::(ModelAPI a m, MonadIO m) => Student -> App a m ()
updateStudHandler student = do
  config <- ask
  lift $ updateStudent student (connection config)

sortStudsHandler :: (ModelAPI a m, MonadIO m) =>Integer -> String -> App a m [Student]
sortStudsHandler seminar group = do
  config <- ask
  lift $ sortedStudents seminar group (connection config)

filterStudsHandler :: (ModelAPI a m, MonadIO m) => String -> App a m [Student]
filterStudsHandler name = do
  config <- ask
  lift $ filterStudents name (connection config)

findAttHandler :: (ModelAPI a m, MonadIO m) => Integer -> App a m (Maybe Attendance)
findAttHandler id = do
  config <- ask
  lift $ findAttendance id (connection config)

switchAttHandler ::(ModelAPI a m, MonadIO m) => Attendance -> App a m ()
switchAttHandler attendance = do
  config <- ask
  lift $ switchAttendance attendance (connection config)

switchActHandler :: (ModelAPI a m, MonadIO m) => Attendance -> App a m ()
switchActHandler attendance = do
	config <- ask
	lift $ switchActivity attendance (connection config)

getAttsHandler :: (ModelAPI a m, MonadIO m) => Integer -> String -> App a m [Attendance]
getAttsHandler seminar group = do
	config <- ask
	lift $ getAttendances seminar group (connection config)

removeAttHandler ::(ModelAPI a m, MonadIO m) => Integer -> App a m ()
removeAttHandler id = do
  config <- ask
  lift $ removeAttendance id (connection config)

updateAttHandler ::(ModelAPI a m, MonadIO m) => Attendance -> App a m ()
updateAttHandler attendance = do
  config <- ask
  lift $ updateAttendance attendance (connection config) 

teamsAttHandler ::(ModelAPI a m, MonadIO m) => Message -> App a m ()
teamsAttHandler message = do
	config <-ask
	lift $ teamsAttendance message (state config) (connection config)

teamsAttendance mess state conn = do
  map <- liftIO $  readTVarIO state
  let studName = (name <$> from) mess
      attValue = fromMaybe Nothing (Map.lookup studName map)

  listOfStudents <- getAllStudents conn
  let tupleOptions = setOption attValue (text mess) listOfStudents
  case fst tupleOptions of
    Nothing -> return ()
    Just att -> case snd tupleOptions of
        "You're all set. See you later."-> do 
                          createAttendance att conn
                          liftIO $ atomically $ modifyTVar state (\x -> Map.delete studName x) 
        _ -> liftIO $ atomically $ modifyTVar state (\x -> Map.insert studName (fst tupleOptions) x) 
  liftIO $ sendRequest (snd tupleOptions) mess
  return ()

filterStudents :: (ModelAPI a m, MonadIO m) => String -> a -> m [Student]
filterStudents nameString conn = do
  allStuds <- getAllStudents conn
  return $ filter (\stud -> isInfixOf (map toLower nameString) (map toLower (_sName stud))) allStuds

sortedStudents :: (ModelAPI a m, MonadIO m) => Integer -> String -> a -> m [Student]
sortedStudents seminar group conn = do
  allStudents    <- getAllStudents conn
  allAttendances <- getAllAttendances conn
  let filteredAttendances = filter 
        (\Attendance{_aGroup=gr, _aSeminar=sem} -> gr==group && seminar>=sem) allAttendances
      studentList = foldl (\lst student@Student{_sId=sid, _sGroup=gr} -> 
        (student, (bonus gr group)+ (getValue seminar (filter (\Attendance{_aStudentId = asid} 
          -> asid == sid) filteredAttendances))):lst) [] allStudents
        where
          bonus ::String -> String -> Integer
          bonus a b = case a == b of
            True -> 100
            _    -> 0
          getValue :: Integer-> [Attendance] -> Integer
          getValue seminar attendances = foldl (\nr Attendance{_aSeminar = sem} -> 
            case seminar - sem of
              0 -> nr + 500
              1 -> nr + 100
              2 -> nr + 50
              3 -> nr + 5
              _ -> nr + 1
            ) 0 attendances
      sortedList = take 40 $ sortBy (flip (compare `on` snd)) studentList
  return $ map fst sortedList


getAttendances ::(ModelAPI a m, MonadIO m) => Integer -> String -> a -> m [Attendance]
getAttendances seminar group conn= do
  allAttendances <- getAllAttendances conn
  return $ filter (\Attendance{_aSeminar = sem, Types._aGroup = gr} -> 
      (sem == seminar) && (gr == group)) allAttendances

switchAttendance :: (ModelAPI a m, MonadIO m) => Attendance -> a -> m()
switchAttendance Attendance{Types._aGroup=gr,_aSeminar=sem,_aStudentId=sid, _aActivity=act} conn= 
  do
    allAttendances <- getAllAttendances conn
    let maybeAtt = find (\Attendance{Types._aGroup=gr2,_aSeminar=sem2,_aStudentId = sid2} ->
                        gr2==gr && sem2==sem && sid ==sid2
                        ) allAttendances
    case maybeAtt of
      Nothing -> createAttendance (Attendance (getMaxId allAttendances) sid sem gr act) conn
      Just Attendance{_aId = aid} -> removeAttendance aid conn
    return ()
    where
      getMaxId :: [Attendance] -> Integer
      getMaxId atts = 1 + foldl (\mid Attendance{_aId = aid} -> 
                                case aid>mid of
                                  True -> aid
                                  _    -> mid
                                ) 0 atts

switchActivity   :: (ModelAPI a m, MonadIO m) => Attendance -> a -> m()
switchActivity Attendance{Types._aGroup=gr,_aSeminar=sem,_aStudentId = sid, _aActivity = act} conn =do
  allAttendances <- getAllAttendances conn
  let maybeAtt = find (\Attendance{_aGroup=gr2,_aSeminar=sem2,_aStudentId = sid2} ->
                      gr2==gr && sem2==sem && sid ==sid2) allAttendances
  case maybeAtt of
      Nothing -> createAttendance (Attendance (getMaxId allAttendances) sid sem gr act) conn
      Just Attendance{_aId = aid} -> updateAttendance (Attendance aid sid sem gr act) conn
  return ()
  where
    getMaxId :: [Attendance] -> Integer
    getMaxId atts = 1 + foldl (\mid Attendance{_aId = aid} -> 
                              case aid>mid of
                                True -> aid
                                _    -> mid
                              ) 0 atts
sendRequest :: String -> Message -> IO ()
sendRequest text Message{mid=id, from=client, conversation=convObj, recipient=bot} = do
    let response = Response text "message" bot client convObj id
        client_secret = "4.Av925w5_tRKiw_j7jQ5_FzdSwn.lUHIf"
        idClient = "a3ec8111-508d-494e-b2e0-2a7d61f7f102"
        urlToken = "https://login.microsoftonline.com/botframework.com/oauth2/v2.0/token"
        urlConversation = ("https://smba.trafficmanager.net/emea/v3/conversations/"
                          ++ (conversationId convObj) ++ "/activities/"++ id)
        body = [ ("grant_type" , "client_credentials")
             , ("client_id" , idClient)
             , ("client_secret" , client_secret)
             , ("scope" ,"https://api.botframework.com/.default")]
    initialReq  <- parseRequest urlConversation
    let request  = initialReq {method = "POST", requestBody = RequestBodyLBS $ encode response}
    man <- newManager tlsManagerSettings
    nakedRequest <- parseRequest urlToken
    body <- responseBody <$> httpLbs (urlEncodedBody body nakedRequest) man
    let token = fromMaybe "" (access_token <$> (decode body :: Maybe Token))
        seqReq = addRequestHeader "Authorization" (packChars ("Bearer " ++ token)) request
    httpLbs seqReq man
    return ()

setOption :: Maybe Attendance -> String -> [Student] -> (Maybe Attendance, String)
setOption maybeAtt command studentList =
  case (init command) of
    "present" ->(Just (Attendance 0 0 0 "" False),typeCode)
    _ -> case maybeAtt of
      Nothing ->(Nothing, invalidCommand)
      Just att -> case _aStudentId att of
        -1 -> case isInfixOf "ie" command of
               True -> case find (\stud -> _sCode stud == command) studentList of
                   Nothing -> (Nothing, invalidCode)
                   Just st -> (Just (Attendance 0 (_sId st) 0 "" False), typeGroup)
               False -> (Nothing , invalidCode)
        _ -> case _aGroup att of
          "" -> case find (== command) groups of
                 Nothing -> (Nothing, invalidGroup)
                 Just gr -> (Just (Attendance 0 (_aStudentId att) 0 gr False), typeSeminar)
          _ -> case find (== command) seminars of
               Nothing -> (Nothing, invalidSeminar)
               Just sem ->(Just (Attendance 0 (_aStudentId att) (read sem ::Integer) (_aGroup att) False), allSet)
  where
    invalidCommand = "Invalid command. Please try again!"
    invalidCode = "Invalid code. Please try again!"
    invalidGroup = "Invalid group. PLease try again!"
    invalidSeminar = "Invalid seminar number. Please try again!"
    typeCode = "Please enter your code."
    typeGroup ="Please enter the group you are attending the seminar with."
    typeSeminar = "Please type the number of the seminar."
    allSet = "You're all set. See you later! :D"
    groups = ["931","932","933","934","935","936","937"]
    seminars= ["1","2","3","4","5","6","7","8","9","10","11","12","13","14"]
