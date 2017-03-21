{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
module Servant.Render.Server where

import Data.Proxy (Proxy(..))
import Servant.Server (HasServer(..))
import Servant.Render (Reflexive(..))

instance HasServer api context => HasServer (Reflexive api) context where
  type ServerT (Reflexive api) m = ServerT api m
  route Proxy ctx d = route (Proxy @api) ctx d
