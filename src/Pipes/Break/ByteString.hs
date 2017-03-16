module Pipes.Break.ByteString (
  -- * Group Producers By A Delimiter
  -- | Break any producer up into groups with the delimiter stripped out.
  --
  -- >>> unBreaksBy delim (breaksBy delim foo) ≡ foo
  --
  -- with the presumption that two 'Producer's are "equivalent" if they produce the same string when drained.
  breakBy ,breaksBy, unBreaksBy,

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

import qualified Data.ByteString.Char8 as B

import Pipes.Break.Internal

-- | Yield argument until it reaches delimiter or end of stream, then return the remainder (minus the delimiter)
endBy :: (Monad m) => B.ByteString -> Producer B.ByteString m r -> Producer B.ByteString m (Producer B.ByteString m r)
endBy = _endBy

endsBy :: (Monad m) => B.ByteString -> Producer B.ByteString m r -> FreeT (Producer B.ByteString m) m r
endsBy = _endsBy

-- | Inverse of 'endsBy'.
unEndsBy :: (Monad m) => B.ByteString -> FreeT (Producer B.ByteString m) m r -> Producer B.ByteString m r
unEndsBy = _unEndsBy

-- | The inverse of 'breaksBy'.
unBreaksBy :: (Monad m) => B.ByteString -> FreeT (Producer B.ByteString m) m r -> Producer B.ByteString m r
unBreaksBy = _unBreaksBy

-- | Group a producer on any delimiter.
breaksBy :: (Monad m) => B.ByteString -> Producer B.ByteString m r -> FreeT (Producer B.ByteString m) m r
breaksBy = _breaksBy

-- | Yield argument until it reaches delimiter or end of stream, then returns the remainder (minus the delimiter)
--
-- >>> rest <- runEffect $ for (breakBy "\r\n" (yield "A\r\nB\r\nC\r\n")) (lift . Prelude.print)
-- "A"
--
-- >>> runEffect $ for rest (lift . print)
-- "B\r\nC\r\n"
--
-- This is almost equivalent to Pipes.ByteString.line except that it works for any delimiter, not just '\n'.
-- It also consumes the delimiter.
breakBy :: (Monad m) => B.ByteString -> Producer B.ByteString m r -> Producer B.ByteString m (Producer B.ByteString m r)
breakBy = _breakBy
