-- | To learn more about pipes-group and how to use these functions with it, check out the in
--   depth tutorial at <http://hackage.haskell.org/package/pipes-group/docs/Pipes-Group-Tutorial.html>

module Pipes.Break.Text (
  -- * Group Producers By A Delimiter
  -- | Break any producer up into groups with the delimiter stripped out with the presumption that
  --   two 'Producer's are "equivalent" if they produce the same string when drained.
  --
  -- @
  --   'unBreaksBy' delim ('breaksBy' delim foo) ≡ foo
  --   'unBreakBy' delim ('breakBy' delim foo) ≡ foo
  -- @
  breakBy, unBreakBy, breaksBy, unBreaksBy,

  -- * Group Producers Ending By A Delimiter
  -- | These are almost equivalent to the breakBy functions, however they imply that every chunk "ends" with a delimiter,
  --   rather than just being separated by them.
  --
  --   Unfortunately it is impossible to know in a streaming fashion for sure that your next group will
  --   ever end with a delimiter (or end at all for that matter). The only way to find out is to store every line you receive until you find it.
  --   Therefore, the endsBy family of functions are not invertible.
  --
  -- >>> mconcat $ Pipes.Prelude.toList (unEndsBy "\r\n" (endsBy "\r\n" (yield "A\r\nB\r\nC")))
  -- "A\r\nB\r\nC\r\n"
  --
  -- In other words:
  --
  -- @
  --  'unEndsBy' delim ('endsBy' delim foo) ≠ foo
  --  'unEndBy' delim ('endBy' delim foo) ≠ foo
  -- @
  endBy, unEndBy, endsBy, unEndsBy,

  -- * Utilities
  replace
) where

import Pipes as P
import Pipes.Group as P

import qualified Data.Text as T

import Pipes.Break.Internal

-- | Yield argument until it reaches delimiter or end of stream, then return the remainder (minus the delimiter).
--
--   This is equivalent to 'breakBy'.
endBy :: (Monad m) => T.Text -> Producer T.Text m r -> Producer T.Text m (Producer T.Text m r)
endBy = breakBy

-- | Recombine a producer that had been previously broken by using a separator.  If the second stream is empty
--   the delimiter will be added on at the end of the first producer anyways.
--
-- >>> mconcat $ Pipes.Prelude.toList (unEndBy "\n" (yield "abc" >> return (yield "def")))
-- "abc\ndef"
--
-- >>> mconcat $ Pipes.Prelude.toList (unEndBy "\n" (yield "abc" >> return (return ())))
-- "abc\n"
--
-- This is /not/ equivalent to 'unBreakBy' and is not quite an inverse of 'endBy'
unEndBy :: (Monad m) => T.Text -> Producer T.Text m (Producer T.Text m r) -> Producer T.Text m r
unEndBy = _unEndBy

-- | Recombine a producer that had been previously broken recombining it with the provided delimiter.
--
-- >>> mconcat $ Pipes.Prelude.toList (unBreakBy "\n" (yield "abc" >> return (yield "def")))
-- "abc\ndef"
--
-- >>> mconcat $ Pipes.Prelude.toList (unBreakBy "\n" (yield "abc" >> return (return ())))
-- "abc"
--
-- The inverse of 'breakBy'
unBreakBy :: (Monad m) => T.Text -> Producer T.Text m (Producer T.Text m r) -> Producer T.Text m r
unBreakBy = _unBreakBy

endsBy :: (Monad m) => T.Text -> Producer T.Text m r -> FreeT (Producer T.Text m) m r
endsBy = _endsBy

-- | Not quite the inverse of 'endsBy'.
unEndsBy :: (Monad m) => T.Text -> FreeT (Producer T.Text m) m r -> Producer T.Text m r
unEndsBy = _unEndsBy

-- | The inverse of 'breaksBy'.
unBreaksBy :: (Monad m) => T.Text -> FreeT (Producer T.Text m) m r -> Producer T.Text m r
unBreaksBy = _unBreaksBy

-- | Group a producer on any delimiter.
breaksBy :: (Monad m) => T.Text -> Producer T.Text m r -> FreeT (Producer T.Text m) m r
breaksBy = _breaksBy

-- | Yield argument until it reaches delimiter or end of stream, then returns the remainder (minus the delimiter).
--
-- >>> rest <- runEffect $ for (breakBy "\r\n" (yield "A\r\nB\r\nC\r\n")) (lift . Prelude.print)
-- "A"
--
-- >>> runEffect $ for rest (lift . print)
-- "B\r\nC\r\n"
--
-- This is almost equivalent to Pipes.Text.line except that it works for any delimiter, not just '\n',
-- and it also consumes the delimiter.
breakBy :: (Monad m) => T.Text -> Producer T.Text m r -> Producer T.Text m (Producer T.Text m r)
breakBy = _breakBy

-- | Replace one delimiter with another in a stream.
--
-- >>> fmap mconcat <$> toListM $ replace "\r\n" "\n" (yield "abc\ndef\r\nfoo\n\r\nbar")
-- "abc\ndef\nfoo\n\nbar"
replace :: (Monad m) => T.Text -> T.Text -> Producer T.Text m r -> Producer T.Text m r
replace from to = unBreaksBy to . breaksBy from
