{-# LANGUAGE RankNTypes #-}

-- | For those who like lenses, here are Lens library versions of the functions from Pipes.Break.ByteString

module Pipes.Break.ByteString.Lens (
  endsBy, unEndsBy
) where

import Pipes as P
import Pipes.Group as P

import qualified Data.ByteString as B

import Pipes.Break.Internal

type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

endsBy :: Monad m => B.ByteString -> Lens' (Producer B.ByteString m r) (FreeT (Producer B.ByteString m) m r)
endsBy del k p' = fmap (_unEndsBy del) (k (_endsBy del p'))


-- | Inverse of 'endsBy'
unEndsBy :: Monad m => B.ByteString -> Lens' (FreeT (Producer B.ByteString m) m r) (Producer B.ByteString m r)
unEndsBy del k p' = fmap (_endsBy del) (k (_unEndsBy del p'))


