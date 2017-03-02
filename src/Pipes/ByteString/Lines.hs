{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PartialTypeSignatures, NoMonomorphismRestriction, DeriveGeneric, Rank2Types #-}

-- | This is equivalent to the <http://hackage.haskell.org/package/pipes-bytestring/docs/Pipes-ByteString.html#v:lines> except it works for lines delimited with "\\r\\n"
--
-- Warning: Since this works on bytestrings, it assumes a particular encoding.  You should use "Pipes.Text.Split" if possible

module Pipes.ByteString.Lines (
  Pipes.ByteString.Lines.lines,
  Pipes.ByteString.Lines.unlines, line
) where

import Pipes as P
import Pipes.Group as P

import qualified Data.ByteString.Char8 as B

import Pipes.Lines

type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

-- | Producer of strict 'Data.ByteString.Char8.ByteString's is delimited by "\\r\\n"
--
-- >>> runEffect $ for (over (lines . individually) (<* yield "!") (yield "asdf\r\nqwerty\r\n")) (lift . print)
-- "asdf"
-- "!"
-- "\r\n"
-- "qwerty"
-- "!"
-- "\r\n"
-- 
lines :: Monad m => Lens' (Producer B.ByteString m r) (FreeT (Producer B.ByteString m) m r)
lines k p' = fmap _unLinesRn (k (_linesRn p'))

unlines :: Monad m => Lens' (FreeT (Producer B.ByteString m) m r) (Producer B.ByteString m r)
unlines k p' = fmap _linesRn (k (_unLinesRn p'))

-- | Producer which consumes a single line, then returns a producer with rest of input.
--
-- >>> rest <- runEffect $ for (line (yield "foo\r\nbar\r\nbaz\r\n")) (lift . print)
-- "foo"
-- >>> runEffect $ for rest (lift . print)
-- "bar\r\nbaz\r\n"
line :: (Monad m) => Producer B.ByteString m r -> Producer B.ByteString m (Producer B.ByteString m r)
line = _lineRn


