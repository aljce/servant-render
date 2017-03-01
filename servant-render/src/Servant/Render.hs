{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Render (
  ServantErr(..),
  Link(..),
  linkView,
  Env(..),
  Render(..),
  HasRender(..),
  SupportsServantRender )where

import Data.Kind (type Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol,symbolVal)
import Control.Monad.Fix (MonadFix)
import Reflex.Class (Reflex(..),MonadSample(..),MonadHold(..))
import Reflex.Dom   (DomBuilder,PostBuild,dyn,Workflow(..),workflowView)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Semigroup ((<>))
import Servant.API ((:<|>)(..),(:>),Capture,Verb,ReflectMethod(..),MimeUnrender(..),
                    ToHttpApiData(..),FromHttpApiData(..))
import Servant.Common.Uri (Authority(..),Uri(..),unconsPathPiece)
import Servant.Common.Req (Req(..),SupportsServantRender,performRequestCT,
                           performOneRequest,prependPathPiece)

data ServantErr = NotFound T.Text | AjaxFailure T.Text

newtype Link t m = Link { unLink :: m (Event t (Uri, Link t m)) }

linkView :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => Link t m -> m (Event t Uri)
linkView l = do
  rec eResult  <- dyn . fmap unLink =<< holdDyn l (fmap snd eReplace)
      eReplace <- fmap switch (hold never eResult)
  return (fmap fst eReplace)

data Env t m = Env
  { envAuthority :: Dynamic t Authority
  , envOnFailure :: ServantErr -> Link t m
  , envFailUri   :: Uri }

data Render api t m = Render
  { renderRouter :: Uri   -> Either ServantErr (Link t m)
  , renderLinks  :: Req t -> Links api t m
  }

class HasRender api t m where
  type Widgets api t m :: Type
  type Links   api t m :: Type
  render :: SupportsServantRender t m => Proxy api -> Env t m -> Widgets api t m -> Render api t m

instance (ReflectMethod method, contents ~ (c:cs), MimeUnrender c a, SupportsServantRender t m) =>
  HasRender (Verb method status contents a) t m where
  type Widgets (Verb method status contents a) t m = a -> Link t m
  type Links   (Verb method status contents a) t m = Event t () -> Link t m
  render Proxy (Env authority onFailure failUri) cb = Render widget link
    where widget req = Right . Link $ do
            url <- sample (current authority)
            res <- performOneRequest contentType url req
            (unLink . either (onFailure . AjaxFailure) cb) res
          link req e = Link $ do
            let method = T.decodeUtf8 (reflectMethod (Proxy @method))
            reqs <- performRequestCT contentType (req { reqMethod = method }) authority e
            return (fmap (either ((failUri,) . onFailure . AjaxFailure) (fmap cb)) reqs)
          contentType = Proxy @c

instance (HasRender a t m, HasRender b t m) => HasRender (a :<|> b) t m where
  type Widgets (a :<|> b) t m = Widgets a t m :<|> Widgets b t m
  type Links   (a :<|> b) t m = Links a t m :<|> Links b t m
  render Proxy env ~(a :<|> b) = Render (widgetA <> widgetB) (\req -> linkA req :<|> linkB req)
    where Render widgetA linkA = render (Proxy @a) env a
          Render widgetB linkB = render (Proxy @b) env b

instance (KnownSymbol path, HasRender api t m) => HasRender (path :> api) t m where
  type Widgets (path :> api) t m = Widgets api t m
  type Links   (path :> api) t m = Links api t m
  render Proxy env cb = Render widgets' (links . prependPathPiece (pure (Right path)))
    where Render widgets links = render (Proxy @api) env cb
          widgets' uri = case unconsPathPiece uri of
            Just (piece, rest) -> case piece == path of
              True  -> widgets rest
              False -> Left (NotFound ("Url path piece: " <> piece <> " doesnt match: " <> path))
            Nothing -> Left (NotFound "Url too short")
          path = T.pack (symbolVal (Proxy @path))

instance (ToHttpApiData a, FromHttpApiData a, HasRender api t m, SupportsServantRender t m) =>
  HasRender (Capture cap a :> api) t m where
  type Widgets (Capture cap a :> api) t m = Widgets api t m
  type Links   (Capture cap a :> api) t m = Dynamic t a -> Links api t m
  render Proxy env cb = Render widgets' links'
    where Render widgets links = render (Proxy @api) env cb
          widgets' uri = case unconsPathPiece uri of
            Just (piece, rest) -> case parseUrlPiece piece :: Either T.Text a of
              Left failure -> Left (NotFound ("Could not parse url becasue: " <> failure))
              Right _      -> widgets rest
            Nothing -> Left (NotFound "Url too short")
          links' req val = links (prependPathPiece (fmap (Right . toUrlPiece) val) req)
















