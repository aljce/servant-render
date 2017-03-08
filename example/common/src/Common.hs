{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Common where

import Data.Proxy (Proxy(..))
import GHC.Generics
import Data.Aeson (FromJSON,ToJSON)
import Servant.API
import Servant.Common.Uri (Uri(..))
import Servant.Render (HasRender(..),Link(..),ServantErr(..),RunTime)
import Reflex.Dom hiding (Link)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Monoid ((<>))

data Item = Item {
  itemId    :: Int,
  itemName  :: String,
  itemPrice :: Double } deriving (Show,Generic,FromJSON,ToJSON)

type API s = "item" :> "all" :> Get '[JSON,RunTime s] [Item]
        :<|> "item" :> "one" :> Capture "itemId" Int :> Get '[JSON,RunTime s] Item
        :<|> Get '[JSON,RunTime s] ()

api :: Proxy (API s)
api = Proxy

item :: MonadWidget t m => Item -> m ()
item (Item i name p) = do
  el "div" $ do
    el "div" $ text $ "Item: "  <> T.pack name
    el "div" $ text $ "Id: "    <> T.pack (show i)
    el "div" $ text $ "Price: " <> T.pack (show p)

widgets :: (MonadWidget t m, MonadIO m) => Links (API s) t m -> Widgets (API s) t m
widgets (jumpAll :<|> jumpOne :<|> jumpHome) =
  displayAll jumpOne jumpHome :<|> displayOne jumpAll jumpHome :<|> displayHome jumpAll jumpOne
  where displayAll jumpOne jumpHome items = Link $ do
          mapM_ (el "div" . item) items
          el "div" $ text "Jump to an item based off of its id: "
          t <- textInput $ def & textInputConfig_inputType .~ "number"
                               & textInputConfig_initialValue .~ "0"
          unLink $ jumpOne (fmap (maybe 0 id . readMaybe . T.unpack) (_textInput_value t))
                           (fmapMaybe (\x -> if keyCodeLookup x == Enter then Just () else Nothing)
                                      (_textInput_keypress t))
        displayOne jumpAll jumpHome i = Link $ do
          item i
          h <- button "Jump home!"
          unLink (jumpHome h)
        displayHome jumpAll jumpOne () = Link $ do
          a <- button "Jump All!"
          unLink (jumpAll a)


errorPage :: (Monad m, DomBuilder t m) => Links (API s) t m -> ServantErr -> Link t m
errorPage (_ :<|> _ :<|> jumpHome) err = Link $ do
  el "div" $ text $ "Something went wrong : " <> displayErr err
  b <- button "Jump home!"
  unLink (jumpHome b)
  where displayErr (NotFound err) = "Not Found: " <> err
        displayErr (AjaxFailure err) = "Ajax failure: " <> err

errorPageLoc :: Uri
errorPageLoc = Uri ["error"] []
