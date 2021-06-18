{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}
import           Prelude ()
import           Prelude.Compat
import qualified Control.Concurrent               as C
import           Control.Concurrent.MVar
import           Control.Exception                (bracket)
import           Control.Lens              hiding (Context)
import           Data.Maybe
import           Data.Either
import           Data.List          
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict              as HM
import           Data.Text                        (Text, unpack)
import           GHC.Generics
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp
import           Servant
import           Servant.Client
import           Servant.Server
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal (serverDoesntSatisfy)
import 					 ServantModule
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher
import Types
import           Control.Monad.STM           (atomically)
import qualified Data.Map as Map
import           Control.Concurrent.STM.TVar (TVar, newTVar)
import JSON
import SQL
main :: IO ()
main = hspec businessLogicSpec

withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action = do
  localAtt <- atomically $ newTVar Map.empty
  Warp.testWithApplication (pure (app $ AppConfig (SQLConnection "gradix.db") localAtt)) action

getStud :: Integer -> ClientM (Maybe Student)
postStud :: Student -> ClientM ()
putStud :: Student -> ClientM ()
deleteStud :: Integer -> ClientM ()
sortStud :: Integer -> String -> ClientM [Student]
filterStud :: String-> ClientM [Student]
getAtt :: Integer -> ClientM (Maybe Attendance)
postAtt :: Attendance -> ClientM ()
putAtt :: Attendance -> ClientM ()
deleteAtt :: Integer -> ClientM ()
getAtts :: Integer -> String -> ClientM [Attendance]
postAcc :: Attendance -> ClientM ()
mess :: Message -> ClientM ()
(getStud :<|> postStud :<|>putStud :<|>deleteStud :<|>sortStud :<|> filterStud :<|> getAtt 
  :<|> postAtt :<|>putAtt :<|> deleteAtt :<|> getAtts :<|> postAcc :<|> mess) = client api

businessLogicSpec :: Spec
businessLogicSpec = around withUserApp $ do 
    let createUser = client (Proxy :: Proxy API)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
    describe "GET /student" $ do
      it "should be able to get a student" $ \port -> do
        result <- runClientM (getStud 190) (clientEnv port)
        result `shouldBe` (Right $ Just $ Student 
            { _sId = 190, _sName = "VINCZI RICHARD"
            , _sGroup = "937", _sCode = "vrie2641"})
    describe "POST /student" $ do
      it "should be able to add a student" $ \port -> do
        result <- runClientM (getStud 1001) (clientEnv port)
        result `shouldBe` (Right Nothing)
        runClientM (postStud (Student 1001 "test" "test" "test")) (clientEnv port)
        result <- runClientM (getStud 1001) (clientEnv port)
        result `shouldBe` (Right $ Just $ (Student 1001 "test" "test" "test"))
    describe "PUT /student" $ do
      it "should be able to update a student" $ \port -> do
        result <- runClientM (getStud 1001) (clientEnv port)
        result `shouldBe` (Right $ Just $ (Student 1001 "test" "test" "test"))
        runClientM (putStud (Student 1001 "updateTest" "test" "test")) (clientEnv port)
        result <- runClientM (getStud 1001) (clientEnv port)
        result `shouldBe` (Right $ Just $ (Student 1001 "updateTest" "test" "test"))
    describe "DELETE /student" $ do
      it "should be able to delete a student" $ \port -> do
        result <- runClientM (getStud 1001) (clientEnv port)
        result `shouldBe` (Right $ Just $ (Student 1001 "updateTest" "test" "test"))
        runClientM (deleteStud 1001) (clientEnv port)
        result <- runClientM (getStud 1001) (clientEnv port)
        result `shouldBe` (Right Nothing)
    describe "GET /attendance" $ do
      it "should be able to get an attendance" $ \port -> do
        result <- runClientM (getAtt (-1)) (clientEnv port)
        result `shouldBe` (Right $ Just $ Attendance
            { _aId = (-1), _aStudentId = (-1)
            , _aGroup = "0", _aSeminar = 0
				  	, _aActivity = False})
    describe "POST /attendance" $ do
      it "should be able to add an attendance" $ \port -> do
        result <- runClientM (getAtts 1 "931") (clientEnv port)
        print result
        let x = length (fromRight [] result)
        runClientM (postAtt (Attendance 1001 1 1 "931" False)) (clientEnv port)
        result <- runClientM (getAtts 1 "931") (clientEnv port)
        let y = length (fromRight [] result)
        y `shouldBe` x+1
        let maybeStud = find (\Attendance{_aStudentId = sid} -> sid == 1) (fromRight [] result)
            attId = fromMaybe (-1000) $ fmap _aId maybeStud 
        result <- runClientM (getAtt attId) (clientEnv port)
        print result
        result `shouldBe` (Right $ Just $ (Attendance attId 1 1 "931" False))
    describe "PUT /attendance" $ do
      it "should be able to update an attendance" $ \port -> do
        result <-runClientM (getAtts 1 "931") (clientEnv port)
        print result
        let maybeStud = find (\Attendance{_aStudentId = sid} -> sid == 1) (fromRight [] result)
            attId = fromMaybe (-1000) $ fmap _aId maybeStud
        result <- runClientM (getAtt attId) (clientEnv port)
        result `shouldBe` (Right $ Just $ (Attendance attId 1 1 "931" False))
        runClientM (putAtt (Attendance attId 1 2 "932" False)) (clientEnv port)
        result <- runClientM (getAtt attId) (clientEnv port)
        print result
        result `shouldBe` (Right $ Just $ (Attendance attId 1 2 "932" False))
    describe "DELETE /attendance" $ do
      it "should be able to delete an attendance" $ \port -> do
        result <- runClientM (getAtts 2 "932") (clientEnv port)
        print result
        let maybeStud = find (\Attendance{_aStudentId = sid} -> sid == 1) (fromRight [] result)
            attId = fromMaybe (-1000) $ fmap _aId maybeStud
        result <- runClientM (getAtt attId) (clientEnv port)
        result `shouldBe` (Right $ Just $ (Attendance attId 1 2 "932" False))
        runClientM (deleteAtt attId) (clientEnv port)
        result <- runClientM (getAtt attId) (clientEnv port)
        print result
        result `shouldBe` (Right Nothing)
