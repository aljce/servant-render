module Reflex.Link where

import Reflex.Class (Reflex(..))
import Reflex.EventWriter (EventWriter(..),EventWriterT(..),runEventWriterT)

import Servant.Common.Uri (Uri(..))

newtype Link t m a = Link { unLink :: EventWriterT t (Uri, Link t m a) m a }

linkView :: Link t m a -> m (Event t a, Event t Uri)
linkView = undefined
