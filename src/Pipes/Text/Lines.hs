{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PartialTypeSignatures, NoMonomorphismRestriction, DeriveGeneric, Rank2Types #-}

-- | This is equivalent to the <http://hackage.haskell.org/package/pipes-text/docs/Pipes-Text.html#v:lines> except it works for lines delimited with "\\r\\n"

module Pipes.Text.Lines (
  Pipes.Text.Lines.lines,
  Pipes.Text.Lines.unlines, line
) where

import Pipes as P
import Pipes.Group as P

import qualified Data.Text as T

import Pipes.Lines

type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

-- | Producer of strict 'Data.Text.Text's is delimited by "\\r\\n"
--
-- >>> runEffect $ for (over (lines . individually) (<* yield "!") (yield "asdf\r\nqwerty\r\n")) (lift . print)
-- "asdf"
-- "!"
-- "\r\n"
-- "qwerty"
-- "!"
-- "\r\n"
lines :: Monad m => Lens' (Producer T.Text m r) (FreeT (Producer T.Text m) m r)
lines k p' = fmap _unLinesRn (k (_linesRn p'))

unlines :: Monad m => Lens' (FreeT (Producer T.Text m) m r) (Producer T.Text m r)
unlines k p' = fmap _linesRn (k (_unLinesRn p'))

-- | Producer which consumes a single line, then returns a producer with rest of input.
--
-- >>> rest <- runEffect $ for (line (yield "foo\r\nbar\r\nbaz\r\n")) (lift . print)
-- "foo"
-- >>> runEffect $ for rest (lift . print)
-- "bar\r\nbaz\r\n"
line :: (Monad m) => Producer T.Text m r -> Producer T.Text m (Producer T.Text m r)
line = _lineRn


