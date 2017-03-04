{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Servant.Common.PopState where

import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(..))
import Reflex.Class (Reflex(..),MonadHold(..),ffor,leftmost)
import Reflex.Dynamic (attachPromptlyDyn,attachPromptlyDynWithMaybe)
import Reflex.PerformEvent.Class (PerformEvent(..))
import Reflex.Dom (HasWebView(..))
import Servant.Common.Uri (Authority(..),Uri(..),encodeUrl)

#if ghcjs_HOST_OS
import Language.Javascript.JSaddle (eval,toJSVal,call,liftJSM)

getPopState :: (Reflex t, MonadHold t m) => m (Event t (Authority, Uri))
getPopState = return never

url :: (MonadHold t m, PerformEvent t m, MonadIO (Performable m)) =>
  Dynamic t Authority -> Event t Uri -> m (Dynamic t Uri)
url authority us = do
  u0 <- pure (Uri [] [])
  _ <- performEvent $ ffor (attachPromptlyDyn authority us) $ \(authority,uri) -> do
    liftJSM $ do
      f <- eval ("(function (url) { window[\"history\"][\"pushState\"](0,\"\",url) })" :: T.Text)
      url <- toJSVal (encodeUrl authority uri)
      _ <- call f f [url]
      return ()
  ps <- attachPromptlyDynWithMaybe (\a1 (a2,uri) -> if a1 == a2 then Just uri else Nothing) authority <$> getPopState
  holdDyn u0 (leftmost [us,ps])
#else
import Reflex.Class (MonadSample(..))
url :: (Reflex t,MonadHold t m) => Dynamic t Authority -> Event t Uri -> m (Dynamic t Uri)
url authority uris = do
  authority0 <- (sample . current) authority
  holdDyn (useAuthorityPath authority0) uris
  where useAuthorityPath (Authority _ _ _ path) = Uri (filter (not . T.null) (T.splitOn "/" path)) []
#endif
