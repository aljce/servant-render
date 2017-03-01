{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ConstraintKinds     #-}
-- This module shamelessly ripped off from servant-reflex
module Servant.Common.Req where

import           Control.Applicative        (liftA2, liftA3)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes)
import           Data.Bifunctor             (Bifunctor(..))
import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy(..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as LT
import           Control.Monad.IO.Class     (MonadIO(..))
import           Reflex.Dom
import           Servant.API.ContentTypes   (MimeUnrender(..), NoContent(..))
import           Web.HttpApiData            (ToHttpApiData(..))
import           Servant.API.BasicAuth
import           Language.Javascript.JSaddle.Types (MonadJSM)
import           Control.Concurrent.MVar    (newEmptyMVar,putMVar,takeMVar)
import           Servant.Common.Uri         (Authority(..),Uri(..),encodeAuthority',encodeAuthority,encodeUri)

data ReqResult a = ResponseSuccess a XhrResponse
                 | ResponseFailure Text XhrResponse
                 | RequestFailure Text deriving Functor

reqSuccess :: ReqResult a -> Maybe a
reqSuccess (ResponseSuccess x _) = Just x
reqSuccess _                     = Nothing

reqFailure :: ReqResult a -> Maybe Text
reqFailure (ResponseFailure s _) = Just s
reqFailure (RequestFailure s)    = Just s
reqFailure _                     = Nothing

response :: ReqResult a -> Maybe XhrResponse
response (ResponseSuccess _ x) = Just x
response (ResponseFailure _ x) = Just x
response _                     = Nothing

reqView :: ReqResult a -> Either T.Text a
reqView (ResponseSuccess a _) = Right a
reqView (ResponseFailure t _) = Left t
reqView (RequestFailure t)    = Left t

performOneRequest :: (MimeUnrender c a, SupportsServantRender t m) =>
  Proxy c -> Authority -> Uri -> m (Either T.Text a)
performOneRequest ct authority uri = do
  var <- liftIO newEmptyMVar
  _ <- newXMLHttpRequest (xhrRequest "GET" encodedUrl headers) (liftIO . putMVar var)
  liftIO (first T.pack . decodeRes <$> takeMVar var)
  where headers = def { _xhrRequestConfig_headers = "Accept" =: "application/json" }
        encodedUrl = LT.toStrict . LT.toLazyText $ encodeAuthority authority <> encodeUri uri
        decodeRes xhrRes = note "No body text" (_xhrResponse_responseText xhrRes) >>= decode
          where decode = mimeUnrender ct . BL.fromStrict . TE.encodeUtf8


-------------------------------------------------------------------------------
-- | You must wrap the parameter of a QueryParam endpoint with 'QParam' to
-- indicate whether the parameter is valid and present, validly absent, or
-- invalid
data QParam a = QParamSome a
              -- ^ A valid query parameter
              | QNone
              -- ^ Indication that the parameter is intentionally absent (the request is valid)
              | QParamInvalid Text
              -- ^ Indication that your validation failed (the request isn't valid)

qParamToQueryPart :: ToHttpApiData a => QParam a -> Either Text (Maybe Text)
qParamToQueryPart (QParamSome a)    = Right (Just $ toQueryParam a)
qParamToQueryPart QNone             = Right Nothing
qParamToQueryPart (QParamInvalid e) = Left e

data QueryPart t = QueryPartParam  (Dynamic t (Either Text (Maybe Text)))
                 | QueryPartParams (Dynamic t [Text])
                 | QueryPartFlag   (Dynamic t Bool)


-------------------------------------------------------------------------------
-- The data structure used to build up request information while traversing
-- the shape of a servant API
data Req t = Req
  { reqMethod    :: Text
  , reqPathParts :: [Dynamic t (Either Text Text)]
  , qParams      :: [(Text, QueryPart t)]
  , reqBody      :: Maybe (Dynamic t (Either Text (BL.ByteString, Text)))
  , headers      :: [(Text, Dynamic t (Either Text Text))]
  , respHeaders  :: XhrResponseHeaders
  , authData     :: Maybe (Dynamic t (Maybe BasicAuthData))
  }

defReq :: Req t
defReq = Req "GET" [] [] Nothing [] def Nothing

prependToPathParts :: Dynamic t (Either Text Text) -> Req t -> Req t
prependToPathParts p req =
  req { reqPathParts = p : reqPathParts req }

addHeader :: (ToHttpApiData a, Reflex t) => Text -> Dynamic t (Either Text a) -> Req t -> Req t
addHeader name val req = req { headers = (name, (fmap . fmap) (TE.decodeUtf8 . toHeader) val) : headers req }


-- * performing requests

displayHttpRequest :: Text -> Text
displayHttpRequest httpmethod = "HTTP " <> httpmethod <> " request"

type SupportsServantRender t m =
  (Reflex t,MonadIO m,MonadJSM m,HasJSContext m,MonadIO (Performable m),MonadJSM (Performable m)
  ,HasJSContext (Performable m),MonadSample t m,TriggerEvent t m,PerformEvent t m)

-- | This function actually performs the request.
performRequest :: forall t m uri. (SupportsServantRender t m)
               => Req t
               -> Dynamic t Authority
               -> Event t ()
               -> m (Event t XhrResponse, Event t Text)
performRequest req reqHost trigger = do

  let t :: Dynamic t [Either Text Text]
      t = sequence $ reverse $ reqPathParts req

      urlParts :: Dynamic t (Either Text [Text])
      urlParts = fmap sequence t

      baseUrl :: Dynamic t (Either Text Text)
      baseUrl = fmap (Right . encodeAuthority') reqHost

      urlPath :: Dynamic t (Either Text Text)
      urlPath = (fmap.fmap) (T.intercalate "/") urlParts

      queryPartString :: (Text, QueryPart t) -> Dynamic t (Maybe (Either Text Text))
      queryPartString (pName, qp) = case qp of
        QueryPartParam p -> ffor p $ \case
          Left e         -> Just (Left e)
          Right (Just a) -> Just (Right $ pName <> "=" <> a)
          Right Nothing  -> Nothing
        QueryPartParams ps -> ffor ps $ \pStrings ->
          if null pStrings
          then Nothing
          else Just $ Right (T.intercalate "&" (fmap (\p -> pName <> "=" <> p) pStrings))
        QueryPartFlag fl -> ffor fl $ \case
          True ->  Just $ Right pName
          False -> Nothing


      queryPartStrings :: [Dynamic t (Maybe (Either Text Text))]
      queryPartStrings = map queryPartString (qParams req)
      queryPartStrings' = fmap (sequence . catMaybes) $ sequence queryPartStrings :: Dynamic t (Either Text [Text])
      queryString :: Dynamic t (Either Text Text) =
        ffor queryPartStrings' $ \qs -> fmap (T.intercalate "&") qs
      xhrUrl =  (liftA3 . liftA3) (\a p q -> a </> if T.null q then p else p <> "?" <> q) baseUrl urlPath queryString
        where
          (</>) :: Text -> Text -> Text
          x </> y | ("/" `T.isSuffixOf` x) || ("/" `T.isPrefixOf` y) = x <> y
                  | otherwise = x <> "/" <> y


      xhrHeaders :: Dynamic t (Either Text [(Text, Text)])
      xhrHeaders = (fmap sequence . sequence . fmap f . headers) req
        where
          f = \(headerName, dynam) ->
                fmap (fmap (\rightVal -> (headerName, rightVal))) dynam

      mkConfigBody :: Either Text [(Text,Text)]
                   -> (Either Text (BL.ByteString, Text))
                   -> Either Text (XhrRequestConfig XhrPayload)
      mkConfigBody ehs rb = case (ehs, rb) of
                  (_, Left e)                     -> Left e
                  (Left e, _)                     -> Left e
                  (Right hs, Right (bBytes, bCT)) ->
                    Right $ XhrRequestConfig
                      { _xhrRequestConfig_sendData = bytesToPayload bBytes
                      , _xhrRequestConfig_headers  =
                                    Map.insert "Content-Type" bCT (Map.fromList hs)
                      , _xhrRequestConfig_user = Nothing
                      , _xhrRequestConfig_password = Nothing
                      , _xhrRequestConfig_responseType = Nothing
                      , _xhrRequestConfig_withCredentials = False
                      , _xhrRequestConfig_responseHeaders = def
                      }

      xhrOpts :: Dynamic t (Either Text (XhrRequestConfig XhrPayload))
      xhrOpts = case reqBody req of
        Nothing    -> ffor xhrHeaders $ \case
                               Left e -> Left e
                               Right hs -> Right $ def { _xhrRequestConfig_headers = Map.fromList hs
                                                       , _xhrRequestConfig_user = Nothing
                                                       , _xhrRequestConfig_password = Nothing
                                                       , _xhrRequestConfig_responseType = Nothing
                                                       , _xhrRequestConfig_sendData = ""
                                                       , _xhrRequestConfig_withCredentials = False
                                                       }
        Just rBody -> liftA2 mkConfigBody xhrHeaders rBody

      mkAuth :: Maybe BasicAuthData -> Either Text (XhrRequestConfig x) -> Either Text (XhrRequestConfig x)
      mkAuth _ (Left e) = Left e
      mkAuth Nothing r  = r
      mkAuth (Just (BasicAuthData u p)) (Right config) = Right $ config
        { _xhrRequestConfig_user     = Just $ TE.decodeUtf8 u
        , _xhrRequestConfig_password = Just $ TE.decodeUtf8 p}

      addAuth :: Dynamic t (Either Text (XhrRequestConfig x))
              -> Dynamic t (Either Text (XhrRequestConfig x))
      addAuth xhr = case authData req of
        Nothing -> xhr
        Just auth -> liftA2 mkAuth auth xhr

      xhrReq = (liftA2 . liftA2) (\p opt -> XhrRequest (reqMethod req) p opt) xhrUrl (addAuth xhrOpts)

  let reqs    = tagPromptlyDyn xhrReq trigger
      okReqs  = fmapMaybe (either (const Nothing) Just) reqs
      badReqs = fmapMaybe (either Just (const Nothing)) reqs

  resps <- performRequestAsync okReqs

  return (resps, badReqs)

#ifdef ghcjs_HOST_OS
type XhrPayload = String
bytesToPayload :: BL.ByteString -> XhrPayload
bytesToPayload = BL.unpack
#else
type XhrPayload = T.Text
bytesToPayload :: BL.ByteString -> XhrPayload
bytesToPayload = T.pack . BL.unpack
#endif

performRequestNoBody :: forall t m uri. (SupportsServantRender t m)
                     => Req t
                     -> Dynamic t Authority
                     -> Event t () -> m (Event t (ReqResult NoContent))
performRequestNoBody req reqHost trigger = do
  (resp, badReq) <- performRequest req reqHost trigger
  return $ leftmost [ fmap (ResponseSuccess NoContent) resp, fmap RequestFailure badReq]


performRequestCT :: (SupportsServantRender t m,
                     MimeUnrender ct a)
                 => Proxy ct -> Req t -> Dynamic t Authority
                 -> Event t () -> m (Event t (ReqResult a))
performRequestCT ct req reqHost trigger = do
  (resp, badReq) <- performRequest req reqHost trigger
  let decodes = ffor resp $ \xhr ->
        ((mimeUnrender ct . BL.fromStrict . TE.encodeUtf8)
         =<< note "No body text" (_xhrResponse_responseText xhr), xhr)
      reqs = ffor decodes $ \case
        (Right a, r) -> ResponseSuccess a r
        (Left e,  r) -> ResponseFailure (T.pack e) r
  return $ leftmost [reqs, fmap RequestFailure badReq]

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right
