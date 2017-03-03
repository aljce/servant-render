{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common
import Reflex.Dom
import Servant.Common.Uri (Authority(..),Scheme(..))
import Servant.Render.Client
import Control.Monad (void)

main :: IO ()
main = mainWidget (void $ serve api localhost widgets errorPageLoc errorPage)
  where localhost = pure (Authority Http "localhost" (Just 8080) "")
