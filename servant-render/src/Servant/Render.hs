{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Servant.Render where

import Data.Kind (type Type)
import Data.Proxy (Proxy(..))
import Reflex.Class (Reflex(..))
import qualified Data.Text as T

import Servant.Common.Req (Req(..))
import Servant.Common.Uri (Authority(..),Uri(..))

data ServantErr = NotFound


newtype Link t m = Link { unLink :: m (Event t (Link t m)) }

data Render api t m = Render
  { renderRouter :: Uri   -> Either ServantErr (Link t m)
  , renderLinks  :: Req t -> Links api t m
  }

class HasRender api t m where
  type Widgets api t m :: Type
  type Links   api t m :: Type
  render :: SupportsServantRender t m => Proxy api -> Dynamic t Authority -> Widgets api t m -> Render api t m
