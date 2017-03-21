{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Proxy
import Common
import Data.List (find)
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import System.Directory
import Servant
import Servant.Render.Server ()
import Network.Wai.Handler.Warp (run)
--If you want to build the example with ghc
import Network.Wai.Middleware.Cors (simpleCors)

main :: IO ()
main = do
  curDir <- getCurrentDirectory
  www <- canonicalizePath (curDir ++ "/../frontend/result/bin/frontend.jsexe")
  var <- newMVar [Item 0 "Apple" 3, Item 1 "Orange" 2, Item 2 "Banana" 9]
  run 8080 (simpleCors (serve (Proxy @(API "" :<|> Raw)) (routes var :<|> serveDirectory www)))
  where routes var = getAllItems var :<|> getOneItem var :<|> getLen var :<|> return ()
        getAllItems = liftIO . readMVar
        getOneItem var iid = do
          items <- liftIO (readMVar var)
          case find (\i -> itemId i == iid) items of
            Just found -> return found
            Nothing    -> throwError err404
        getLen = fmap length . liftIO . readMVar

