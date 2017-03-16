{-# LANGUAGE RankNTypes #-}

{- | For those who like lenses, here are Lens library versions of the functions from "Pipes.Break.Text"

     To learn more about using lenses to manipulate pipes, check out the in depth tutorial at
     <http://hackage.haskell.org/package/pipes-group/docs/Pipes-Group-Tutorial.html>

     Due to the fact that the endsBy family of functions are non invertible, it doesn't make much sense to encourage their use
     as lenses.
     For example, if your protocol were blocks of lines delimited by a double newline at the end (such as in email or http).

>>> fmap mconcat <$> P.toListM $
>>>   over (endsBy "\n\n" . individually) (over (endsBy "\n" . individually) id)
>>>        (yield "A\nB\n\nA\nB\nC\n\n")
"A\nB\n\n\nA\nB\nC\n\n\n\n\n"

  As you can see, this would result in the wrong number of newlines being appended when reforming.
-}

module Pipes.Break.ByteString.Lens (
  -- * Grouping producers by a delimiter
  --
  -- $breakbyoverview
  breaksBy, unBreaksBy,
) where

import Pipes as P
import Pipes.Group as P

import qualified Data.ByteString as B

import Pipes.Break.Internal

type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

{- $breakbyoverview

>>> fmap mconcat <$> P.toListM $
>>>  over (breaksBy "\n\n" . individually) (over (breaksBy "\n" . individually) (<* yield "!"))
>>>    (P.each ["A\nB","\n\n","A","","\nB\nC","\n\n"])
"A!\nB!\n\nA!\nB!\nC!\n\n"
-}


breaksBy :: Monad m => B.ByteString -> Lens' (Producer B.ByteString m r) (FreeT (Producer B.ByteString m) m r)
breaksBy del k p' = fmap (_unBreaksBy del) (k (_breaksBy del p'))


unBreaksBy :: Monad m => B.ByteString -> Lens' (FreeT (Producer B.ByteString m) m r) (Producer B.ByteString m r)
unBreaksBy del k p' = fmap (_breaksBy del) (k (_unBreaksBy del p'))
