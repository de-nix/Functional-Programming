{-# LANGUAGE  OverloadedStrings #-}

module Main where

import Control.Monad.STM           (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Data.Aeson
import Data.Bool
import Data.Map                    as Map                                              
import SQL
import JSON
import ServantModule
import Types

main :: IO ()
main = do
    state <- atomically $ newTVar Map.empty
    runServer $ AppConfig (SQLConnection "gradix.db") state
    
