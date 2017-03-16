{-# LANGUAGE LambdaCase, OverloadedStrings, PartialTypeSignatures, NoMonomorphismRestriction, DeriveGeneric, Rank2Types #-}

module Pipes.Break.Internal (
  _breaksBy, _unBreaksBy, _breakBy,
  _endsBy, _unEndsBy,
  _unEndBy, _unBreakBy
) where


import Data.String (IsString)

import Pipes as P
import Pipes.Group as P
import Pipes.Parse as P

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

-- basically a P.Parser, but it is itself a producer, thus it can yield immediately.
type ParserP a m r = forall x. StateT (Producer a m x) (Producer a m) r

yieldP :: Monad m => a -> ParserP a m ()
yieldP = lift . yield

drawP :: Monad m => ParserP a m (Maybe a)
drawP = hoist lift draw

unDrawP :: Monad m => a -> ParserP a m ()
unDrawP = hoist lift . unDraw

class (Eq a, IsString a) => TextLike a where
  tlNull :: a -> Bool
  tlBreakSubstring :: a -> a -> (a, a)
  tlLength :: a -> Int
  tlTake :: Int -> a -> a
  tlDrop :: Int -> a -> a
  tlIsPrefixOf :: a -> a -> Bool

instance TextLike B.ByteString where
  tlNull = B.null
  tlBreakSubstring = B.breakSubstring
  tlLength = B.length
  tlTake = B.take
  tlDrop = B.drop
  tlIsPrefixOf = B.isPrefixOf

instance TextLike T.Text where
  tlNull = T.null
  tlBreakSubstring = T.breakOn
  tlLength = T.length
  tlTake = T.take
  tlDrop = T.drop
  tlIsPrefixOf = T.isPrefixOf

_breaksBy, _endsBy :: (TextLike a, Monad m) => a -> Producer a m r -> FreeT (Producer a m) m r
_breaksBy = toFreeT . _breakBy
_endsBy = toFreeT . _breakBy

_unBreaksBy, _unEndsBy :: (TextLike a, Monad m) => a -> FreeT (Producer a m) m r -> Producer a m r

_unBreaksBy = intercalates . yield
_unEndsBy del = concats . maps (<* yield del)

_breakBy :: (TextLike a, Monad m) => a -> Producer a m r -> Producer a m (Producer a m r)
-- If the user supplied an empty delimiter, breakByP would infinitely loop.
_breakBy delim | tlNull delim = fmap return
_breakBy delim = execStateT (breakByP delim)


_unEndBy :: (TextLike a, Monad m) => a -> Producer a m (Producer a m r) -> Producer a m r
_unEndBy delim p = p <* yield delim >>= \p' -> p'

_unBreakBy :: (TextLike a, Monad m) => a -> Producer a m (Producer a m r) -> Producer a m r
_unBreakBy delim p = p >>= lift . next >>= \case
  Left r -> return r
  Right (bs, p') -> yield delim >> (yield bs >> p')

-- | Group a producer of bytestrings into a series of producers delimited by f, where the delimiter is dropped
toFreeT :: (TextLike a, Monad m) => (Producer a m r -> Producer a m (Producer a m r)) -> Producer a m r -> FreeT (Producer a m) m r
toFreeT f = FreeT . go0
  where
    go0 p = do
      next p >>= \case
        Left r       -> return (Pure r)
        Right (bs, p') -> return $ Free (go1 (yield bs >> p'))

    go1 p = f p >>= return . FreeT . go0

-- Yield data from underlying producer before the delimiter, while stripping the delimeter out.
breakByP :: (TextLike a, Monad m) => a -> ParserP a m ()
breakByP str = go
  where
    go = 
      drawP >>= \case
        Nothing -> return ()

        Just bs | tlNull bs -> go
        Just bs -> case tlBreakSubstring str bs of
 
           -- null suff means non null pref has no delimeter or partial delimiter so we need to fetch more chunks to
           -- know whether rest of delimiter is incoming.
           (_, suff) | tlNull suff -> if (tlLength str <= 1)

              -- If the delimiter is only one character, we know it can't be in this chunk.
              then yieldP bs >> go

              -- Starting with one less than the length of delimiter, test the end of this chunk.
              else hoist lift (chunkEndsWith str bs (max (tlLength bs - (tlLength str - 1)) 0)) >>= \case

                -- The end of this chunk does not begin with the delimiter, get more chunks, keep going.
                Nothing -> yieldP bs >> go

                -- This chunk has a delimiter at index n, yield up to it, drop remainder of delimiter from rest of stream.
                Just n -> do
                  yieldP (tlTake n bs)
                  hoist lift (dropChars (tlLength str - (tlLength bs - n)))

           -- non null suff means suff has delimiter.
           -- pref must be yielded
           (pref, suff) -> do
              yieldP pref
              unDrawP (tlDrop (tlLength str) suff)

-- Drop n characters from the stream.
dropChars :: (TextLike a, Monad m) => Int -> P.Parser a m ()
dropChars 0 = return ()
dropChars n = draw >>= \case
  Nothing -> return ()
  Just bs | n > tlLength bs -> dropChars $! (n - tlLength bs)
  Just bs -> unDraw (tlDrop n bs)

-- See if Producer with initial chunk ends with the delimiter anywhere after first n characters,
-- and if it does, return number of characters into this chunk where said delimiter began.
chunkEndsWith :: (TextLike a, Monad m) => a -> a -> Int -> P.Parser a m (Maybe Int)
chunkEndsWith str = go
  where go bs n | n >= tlLength bs = return Nothing
        go bs n = startsWith str (tlDrop n bs) >>= \case
          True -> return (Just n)
          False -> go bs $! (n + 1)

-- if first chunk and rest of Producer with rest starts with a, return True.  Never advances the stream.
startsWith :: (TextLike a, Monad m) => a -> a -> P.Parser a m Bool
startsWith = go1
  where
    go0 str = do
      draw >>= \case
        Nothing -> return False
        Just bs | tlNull bs -> go0 str
        Just bs -> go1 str bs <* unDraw bs

    go1 str bs | tlNull bs = go0 str

    go1 str bs | str `tlIsPrefixOf` bs = return True

    go1 str bs | tlLength bs < tlLength str && tlTake bsLen str == bs =
      go0 (tlDrop bsLen str)
      where
        bsLen = tlLength bs
      
    go1 _ _ = return False
