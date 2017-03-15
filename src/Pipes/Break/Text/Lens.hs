{-# LANGUAGE RankNTypes #-}

-- | For those who like lenses, here are Lens library versions of the functions from Pipes.Break.Text

module Pipes.Break.Text.Lens (
  endsBy, unEndsBy
) where

import Pipes as P
import Pipes.Group as P

import qualified Data.Text as T

import Pipes.Break.Internal

type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

endsBy :: Monad m => T.Text -> Lens' (Producer T.Text m r) (FreeT (Producer T.Text m) m r)
endsBy del k p' = fmap (_unEndsBy del) (k (_endsBy del p'))

-- | Inverse of 'endsBy'
unEndsBy :: Monad m => T.Text -> Lens' (FreeT (Producer T.Text m) m r) (Producer T.Text m r)
unEndsBy del k p' = fmap (_endsBy del) (k (_unEndsBy del p'))

