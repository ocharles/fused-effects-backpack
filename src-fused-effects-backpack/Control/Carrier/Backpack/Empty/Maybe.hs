{-# language BlockArguments, FlexibleContexts, FlexibleInstances, LambdaCase,
      MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Control.Carrier.Backpack.Empty.Maybe where

import Control.Algebra
import Control.Effect.Empty
import qualified Control.Carrier.Backpack.Empty.Maybe.Base as Base

type M = EmptyT

-- We could also write: newtype EmptyT a = EmptyT { runEmpty :: MaybeT Base.M a }
newtype EmptyT a = EmptyT { runEmpty :: Base.M (Maybe a) }

instance Functor EmptyT where
  fmap f (EmptyT m) = EmptyT $ fmap (fmap f) m

instance Applicative EmptyT where
  pure = EmptyT . pure . Just
  EmptyT f <*> EmptyT x = EmptyT do
    f >>= \case
      Nothing -> return Nothing
      Just f' -> x >>= \case
        Nothing -> return Nothing
        Just x' -> return (Just (f' x'))

instance Monad EmptyT where
  return = pure
  EmptyT x >>= f = EmptyT do
    x >>= \case
      Just x' -> runEmpty (f x')
      Nothing -> return Nothing

instance Algebra sig Base.M => Algebra (Empty :+: sig) EmptyT where
  alg handle sig context = case sig of
    L Empty -> EmptyT $ return Nothing
    R other -> EmptyT $ thread (maybe (pure Nothing) runEmpty ~<~ handle) other (Just context)
