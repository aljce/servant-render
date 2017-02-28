{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Servant.Common.Req where

import Control.Applicative (liftA2)
import Reflex.Class (Reflex(..),fanEither,ffor)
import Reflex.Dynamic (tagPromptlyDyn)
import Reflex.Dom (def,Performable(..),HasJSContext,PerformEvent,TriggerEvent)

import Reflex.Dom.Xhr (XhrResponseHeaders,XhrResponse,XhrRequest,xhrRequest,performRequestAsync)
import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Network.HTTP.Media as M

import Servant.API.BasicAuth (BasicAuthData)
import Servant.Common.Uri (Authority)

data QueryPart t
  = QueryPartParam  (Behavior t (Either T.Text T.Text))
  | QueryPartParams (Behavior t [T.Text])
  | QueryPartFlag   (Behavior t Bool)

data Req t = Req
  { reqMethod      :: T.Text
  , reqPathParts   :: [Behavior t (Either T.Text T.Text)]
  , reqQuerys      :: [(T.Text, QueryPart t)]
  , reqBody        :: Maybe (Behavior t (Either T.Text (BS.ByteString, M.MediaType)))
  , reqHeaders     :: [(T.Text, Behavior t (Either T.Text T.Text)) ]
  , reqRespHeaders :: XhrResponseHeaders
  , reqAuthData    :: Maybe (Behavior t (Maybe BasicAuthData)) }

defReq :: Req t
defReq = Req "GET" [] [] Nothing [] def Nothing

prependPathPiece :: Behavior t (Either T.Text T.Text) -> Req t -> Req t
prependPathPiece part req = req { reqPathParts = part : reqPathParts req }

type SupportsServantRender t m =
  (MonadIO (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m)

performRequestCollectError :: forall t m. (SupportsServantRender t m) =>
  Req t -> Dynamic t Authority -> Event t () -> m (Event t T.Text, Event t XhrResponse)
performRequestCollectError req baseUrl trigger = (badReqs,) <$> performRequestAsync okReqs
  where xhrUrl  = undefined
        xhrOpts = undefined
        -- reqs = (liftA2 . liftA2) (xhrRequest (reqMethod req)) xhrUrl xhrOpts
        
        reqs :: Dynamic t (Either T.Text (XhrRequest ()))
        reqs = ffor baseUrl $ \url -> pull $ do
          return undefined
        (badReqs, okReqs) = fanEither (tagPromptyDyn reqs trigger)
