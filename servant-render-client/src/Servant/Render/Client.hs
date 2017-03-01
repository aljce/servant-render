{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Render.Client where

import Data.Proxy (Proxy)
import Reflex.Class (Reflex(..),MonadHold)
import Reflex.Dom (dyn,DomBuilder,PostBuild)
import Control.Monad.Fix (MonadFix)
import Servant.Common.Uri (Authority(..),Uri(..))
import Servant.Common.Req (Req,defReq)
import Servant.Render (HasRender(..),Link,linkView,Render(..),Env(..),ServantErr,SupportsServantRender)

serve :: forall api t m.
  (HasRender api t m, MonadFix m, MonadHold t m
  ,DomBuilder t m, PostBuild t m, SupportsServantRender t m) =>
  Proxy api ->
  Dynamic t Authority ->
  (Links api t m -> Widgets api t m) ->
  (Links api t m -> ServantErr -> Link t m) ->
  m (Event t ())
serve api authority makeWidgets makeErrorPage = do
  let (Render widgets makeLinks) = render api (Env authority errorPage) (makeWidgets links) :: Render api t m
      links     = makeLinks defReq
      errorPage = makeErrorPage links
  uri <- popState
  updates <- dyn (fmap (linkView . either errorPage id . widgets) uri)
  return (coincidence updates)
