{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PartialTypeSignatures, NoMonomorphismRestriction, DeriveGeneric, Rank2Types #-}

-- This is equivalent to the <http://hackage.haskell.org/package/pipes-text/docs/Pipes-Text.html#v:lines> except it works for lines delimited with "\\r\\n"

module Pipes.Break.Text (
  -- * Group Producers By A Delimiter
  -- | Break any producer up into groups with the delimiter stripped out.
  --
  -- >>> unBreaksBy delim (breaksBy delim foo) ≡ foo
  --
  -- with the presumption that two 'Producer's are "equivalent" if they produce the same string when drained.
  breakBy, breaksBy, unBreaksBy,

  -- * Group Producers Ending By A Delimiter
  -- | These are almost equivalent to the breakBy functions, however they imply that every chunk "ends" with a delimiter,
  --   rather than just being separated by them.
  --
  --   Unfortunately it is impossible to know in a streaming fashion for sure that your next group will
  --   ever end with a delimiter (or end at all for that matter). The only way to find out is to store every line you receive until you find it.
  --   Therefore, the endsBy family of functions are not invertible.
  --
  -- >>> > Pipes.Prelude.toList (unEndsBy "\r\n" (endsBy "\r\n" (yield "A\r\nB\r\nC")))
  -- >>> ["A\r\nB\r\nC\r\n"]
  --
  -- In other words:
  --
  -- >>> unEndsBy delim (endsBy delim foo) ≠ foo
  --
  -- unless foo happens to end with delim.
  endBy, endsBy, unEndsBy

) where

import Pipes as P
import Pipes.Group as P

import qualified Data.Text as T

import Pipes.Break.Internal

-- | Yield argument until it reaches delimiter or end of stream, then return the remainder (minus the delimiter)
endBy :: (Monad m) => T.Text -> Producer T.Text m r -> Producer T.Text m (Producer T.Text m r)
endBy = _endBy

endsBy :: (Monad m) => T.Text -> Producer T.Text m r -> FreeT (Producer T.Text m) m r
endsBy = _endsBy

-- | Inverse of 'endsBy'.
unEndsBy :: (Monad m) => T.Text -> FreeT (Producer T.Text m) m r -> Producer T.Text m r
unEndsBy = _unEndsBy

-- | The inverse of 'breaksBy'.
unBreaksBy :: (Monad m) => T.Text -> FreeT (Producer T.Text m) m r -> Producer T.Text m r
unBreaksBy = _unBreaksBy

-- | Group a producer on any delimiter.
breaksBy :: (Monad m) => T.Text -> Producer T.Text m r -> FreeT (Producer T.Text m) m r
breaksBy = _breaksBy

-- | Yield argument until it reaches delimiter or end of stream, then returns the remainder (minus the delimiter)
--
-- >>> rest <- runEffect $ for (breakBy "\r\n" (yield "A\r\nB\r\nC\r\n")) (lift . Prelude.print)
-- "A"
--
-- >>> runEffect $ for rest (lift . print)
-- "B\r\nC\r\n"
--
-- This is almost equivalent to Pipes.Text.line except that it works for any delimiter, not just '\n'.
-- It also consumes the delimiter.
breakBy :: (Monad m) => T.Text -> Producer T.Text m r -> Producer T.Text m (Producer T.Text m r)
breakBy = _breakBy
