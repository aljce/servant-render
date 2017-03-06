{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Proxy
import Common
import Data.List (find)
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Servant
import Network.Wai.Handler.Warp (run)

--TODO: Kill me
import Network.Wai.Middleware.Cors (simpleCors)

main :: IO ()
main = do
  var <- newMVar [Item 0 "Apple" 3, Item 1 "Orange" 2, Item 2 "Banana" 9]
  run 8080 (simpleCors (serve (Proxy @(API :<|> Raw)) ((getAllItems var :<|> getOneItem var :<|> return ()) :<|> static)))
  where getAllItems = liftIO . readMVar
        getOneItem var iid = do
          items <- liftIO (readMVar var)
          case find (\i -> itemId i == iid) items of
            Just item -> return item
            Nothing   -> throwError err404
        static = serveDirectory "../frontend/dist/build/frontend/frontend.jsexe"

