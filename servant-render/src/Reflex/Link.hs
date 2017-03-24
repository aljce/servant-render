{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Link (Link(..),linkView,tellLink,addLink,execLinks) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Control.Monad.Fix (MonadFix)
import Reflex.Class (Reflex(..),MonadHold(..),leftmost)
import Reflex.DynamicWriter (MonadDynamicWriter(..),DynamicWriterT,runDynamicWriterT)
import Reflex.Dom (DomBuilder(..),PostBuild(..),dyn)
import Servant.Common.Uri (Uri)

newtype Link t m = Link { unLink :: m (Event t (Uri, Link t m)) }

instance (Reflex t, Applicative m) => Semigroup (Link t m) where
  (Link m1) <> (Link m2) = Link ((\e1 e2 -> leftmost [e1,e2]) <$> m1 <*> m2)
  sconcat (x:|xs) = mconcat (x:xs)

instance (Reflex t, Applicative m) => Monoid (Link t m) where
  mempty = Link (pure never)
  mappend = (<>)
  mconcat links = Link (leftmost <$> traverse unLink links)

linkView :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => Link t m -> m (Event t Uri)
linkView l = do
  rec eResult  <- dyn . fmap unLink =<< holdDyn l (fmap snd eReplace)
      eReplace <- fmap switch (hold never eResult)
  return (fmap fst eReplace)

tellLink :: (Reflex t, MonadDynamicWriter t (Link t n) m) => Link t n -> m ()
tellLink = tellDyn . pure

addLink :: (Reflex t, MonadDynamicWriter t (Link t n) m) => (Event t unit -> Link t n) -> m (Event t unit) -> m ()
addLink f action = action >>= tellLink . f

execLinks :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  DynamicWriterT t (Link t m) m () -> Link t m
execLinks writer = Link $ do
  (_, dynLinks) <- runDynamicWriterT writer
  links <- dyn (fmap unLink dynLinks)
  switch <$> hold never links

