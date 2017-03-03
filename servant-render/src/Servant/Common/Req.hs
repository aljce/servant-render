{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Servant.Common.Req where

import Data.Proxy (Proxy(..))
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT (toStrict)
import qualified Data.Text.Lazy.Builder as LT
import Data.Monoid ((<>))
import qualified Data.Map.Strict as M
import Data.Bifunctor (Bifunctor(..))
import Data.Functor.Compose (Compose(..))
import Control.Applicative (liftA2)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Language.Javascript.JSaddle.Types (MonadJSM)
import Control.Monad.IO.Class (MonadIO(..))
import Reflex.Dom
import Servant.API (MimeUnrender(..),BasicAuthData(..))
import Servant.Common.Uri (Authority(..),Uri(..),QueryPiece(..),encodeAuthority,encodeUrl)

note :: e -> Maybe a -> Either e a
note err Nothing = Left err
note _  (Just x) = Right x

decodeXhrRes :: (MimeUnrender c a) => Proxy c -> XhrResponse -> Either T.Text a
decodeXhrRes ct xhrRes =
  note "No body text" (_xhrResponse_responseText xhrRes) >>= first T.pack . decode
  where decode = mimeUnrender ct . LB.fromStrict . T.encodeUtf8

performOneRequest :: (MimeUnrender c a, SupportsServantRender t m) =>
  Proxy c -> Authority -> Uri -> m (Either T.Text a)
performOneRequest ct authority uri = do
  var <- liftIO newEmptyMVar
  _ <- newXMLHttpRequest (xhrRequest "GET" (encodeUrl authority uri) headers) (liftIO . putMVar var)
  liftIO (decodeXhrRes ct <$> takeMVar var)
  where headers = def { _xhrRequestConfig_headers = "Accept" =: "application/json" }

-------------------------------------------------------------------------------
-- The data structure used to build up request information while traversing
-- the shape of a servant API
data Req t = Req
  { reqMethod      :: T.Text
  , reqPathParts   :: [Dynamic t (Either T.Text T.Text)]
  , reqQueryParams :: [Dynamic t (Either T.Text QueryPiece)]
  , reqBody        :: Maybe (Dynamic t (Either T.Text (T.Text, T.Text)))
  , reqHeaders     :: [(T.Text, Dynamic t (Either T.Text T.Text))]
  , reqRespHeaders :: XhrResponseHeaders
  , reqAuthData    :: Maybe (Dynamic t (Maybe BasicAuthData))
  }

defReq :: Req t
defReq = Req "GET" [] [] Nothing [] def Nothing

prependPathPiece :: Dynamic t (Either T.Text T.Text) -> Req t -> Req t
prependPathPiece p req =
  req { reqPathParts = p : reqPathParts req }

prependQueryParam :: Dynamic t (Either T.Text QueryPiece) -> Req t -> Req t
prependQueryParam q req =
  req { reqQueryParams = q : reqQueryParams req }

type SupportsServantRender t m =
  (Reflex t,MonadIO m,MonadJSM m,HasJSContext m,MonadIO (Performable m),MonadJSM (Performable m)
  ,HasJSContext (Performable m),MonadSample t m,TriggerEvent t m,PerformEvent t m)

performRequest :: forall t m. (SupportsServantRender t m) =>
  Req t -> Dynamic t Authority -> Event t () -> m (Event t (Either T.Text (Uri,XhrResponse)))
performRequest req authority trigger =
  fmap getCompose <$> performRequestsAsync (Compose <$> tagPromptlyDyn (liftAA2 (,) uri reqs) trigger)
  where liftAA2 = liftA2 . liftA2
        reqs :: Dynamic t (Either T.Text (XhrRequest T.Text))
        reqs = liftAA2 (XhrRequest (reqMethod req))
                       (liftA2 (\a -> fmap (encodeUrl a)) authority uri)
                       config
        uri :: Dynamic t (Either T.Text Uri)
        uri = liftAA2 Uri (pieces (reqPathParts req)) (pieces (reqQueryParams req))
          where pieces = fmap (fmap reverse . sequence) . sequence
        config :: Dynamic t (Either T.Text (XhrRequestConfig T.Text))
        config = do
          headersE    <- headersD
          authInfoE   <- authInfoD
          bodyE       <- bodyD
          return $ do
            headers     <- headersE
            (user,pass) <- authInfoE
            (body,mct)  <- bodyE
            let m = M.fromList headers
                headers' = maybe m (\ct -> M.insert "Content-Type" ct m) mct
            return (XhrRequestConfig headers' user pass Nothing body False def)
        headersD :: Dynamic t (Either T.Text [(T.Text, T.Text)])
        headersD = (fmap sequence . traverse tup . reqHeaders) req
          where tup (headerName,d) = fmap (fmap (\v -> (headerName,v))) d
        authInfoD :: Dynamic t (Either T.Text (Maybe T.Text, Maybe T.Text))
        authInfoD = case reqAuthData req of
          Just authD -> ffor authD $ \case
            Just (BasicAuthData user pass) -> do
              userE <- safeDecode user
              passE <- safeDecode pass
              return (Just userE,Just passE)
            Nothing -> nada
          Nothing -> pure nada
          where safeDecode = first (T.pack . show) . T.decodeUtf8'
                nada = Right (Nothing, Nothing)
        bodyD :: Dynamic t (Either T.Text (T.Text, Maybe T.Text))
        bodyD = case reqBody req of
          Just body -> (fmap . fmap . fmap) Just body
          Nothing -> pure (Right ("", Nothing))

performRequestCT :: (MimeUnrender c a, SupportsServantRender t m) =>
  Proxy c -> Req t -> Dynamic t Authority -> Event t () -> m (Event t (Either T.Text (Uri,a)))
performRequestCT ct req authority = fmap (fmap parseRes) . performRequest req authority
  where parseRes res = do
          (uri,xhr) <- res
          a <- decodeXhrRes ct xhr
          return (uri,a)
