{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Servant.Common.PopState where

import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(..))
import Reflex.Class (Reflex(..),MonadHold(..),ffor,leftmost)
import Reflex.Dynamic (attachPromptlyDyn,attachPromptlyDynWithMaybe)
import Reflex.PerformEvent.Class (PerformEvent(..))
import Reflex.Dom (HasWebView(..))
import Servant.Common.Uri (Authority(..),Uri(..),encodeUrl,parseUrl)
import Data.Attoparsec.Text (maybeResult,parse,parseOnly)
import Reflex.TriggerEvent.Class (TriggerEvent)
import Reflex.Dom.Builder.Immediate (wrapDomEventMaybe)
import Language.Javascript.JSaddle (eval,toJSVal,call,liftJSM,MonadJSM)
import JSDOM (currentWindowUnchecked)
import JSDOM.Types (FromJSVal(..))
import JSDOM.EventM (on)
import JSDOM.Generated.Window (popState,getLocation)
import JSDOM.Generated.Location (toString)

--TODO: kill me
import Servant.Common.Uri (encodeUri)
import Reflex (traceEventWith)

getPopState :: (Reflex t, TriggerEvent t m, MonadJSM m) => m (Event t (Authority, Uri))
getPopState = do
  window <- currentWindowUnchecked
  wrapDomEventMaybe window (`on` popState) $ do
    Just loc <- getLocation window
    locStr   <- toString loc
    (return . hush . parseOnly parseUrl) locStr
  where hush (Left _)  = Nothing
        hush (Right x) = Just x

url :: (MonadHold t m, TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m), MonadJSM m) =>
  Dynamic t Authority -> Event t Uri -> m (Dynamic t Uri)
url authority us = do
  u0 <- liftJSM $ do
    window   <- currentWindowUnchecked
    Just loc <- getLocation window
    locStr   <- toString loc
    let parseUrl0 = either (error . ("No parse of window location because: " ++)) id . parseOnly parseUrl
    (return . snd . parseUrl0) locStr
  performEvent_ $ ffor (attachPromptlyDyn authority us) $ \(authority,uri) -> liftJSM $ do
    f <- eval ("(function (url) { window[\"history\"][\"pushState\"](0,\"\",url) })" :: T.Text)
    url <- toJSVal (encodeUrl authority uri)
    _ <- call f f [url]
    return ()
  ps <- getPopState
  holdDyn u0 (traceEventWith (show . encodeUri) (fmap snd ps))
