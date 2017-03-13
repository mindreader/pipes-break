{-# LANGUAGE LambdaCase, OverloadedStrings, PartialTypeSignatures, NoMonomorphismRestriction, DeriveGeneric, Rank2Types #-}

module Pipes.Lines (
  _unLinesRn, _linesRn, _lineRn
) where

import Data.String (IsString)
import Data.Monoid
import Debug.Trace

import Pipes as P
import Pipes.Prelude as P (toList)
import Pipes.Group as P
import Pipes.Parse as P

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import Control.Monad (when, (>=>), unless, void)

type ParserP a m r = forall x. StateT (Producer a m x) (Producer a m) r

yieldP :: Monad m => a -> ParserP a m ()
yieldP = lift . yield

skipP :: Monad m => ParserP a m Bool
skipP = hoist lift skip

drawP :: Monad m => ParserP a m (Maybe a)
drawP = hoist lift draw

unDrawP :: Monad m => a -> ParserP a m ()
unDrawP = hoist lift . unDraw

class (Show a, Monoid a, Eq a, IsString a) => TextLike a where
  tlNull :: a -> Bool
  tlBreakSubstring :: a -> a -> (a, a)
  tlLength :: a -> Int
  tlTake :: Int -> a -> a
  tlDrop :: Int -> a -> a
--  tlHead :: a -> Char
--  tlLast :: a -> Char
  tlIsPrefixOf :: a -> a -> Bool
--  tlInit :: a -> a

instance TextLike B.ByteString where
  tlNull = B.null
  tlBreakSubstring = B.breakSubstring
  tlLength = B.length
  tlTake = B.take
  tlDrop = B.drop
--  tlHead = B.head
--  tlLast = B.last
  tlIsPrefixOf = B.isPrefixOf
--  tlInit = B.init

instance TextLike T.Text where
  tlNull = T.null
  tlBreakSubstring = T.breakOn
  tlLength = T.length
  tlTake = T.take
  tlDrop = T.drop
--  tlHead = T.head
--  tlLast = T.last
  tlIsPrefixOf = T.isPrefixOf
--  tlInit = T.init

_unLinesRn ::(TextLike a, Monad m) => FreeT (Producer a m) m r -> Producer a m r
-- _unLinesRn = concats . maps (<* yield "\r\n")
_unLinesRn = intercalates (yield "\r\n") -- concats . maps (<* yield "\r\n")

-- | Group a producer of bytestrings into a series of producers delimited by "\r\n"
--   The \r\n are dropped.
_linesRn :: (TextLike a, Monad m) => Producer a m r -> FreeT (Producer a m) m r
_linesRn = FreeT . go0
  where
    go0 p = do
      next p >>= \case
        Left r       -> return (Pure r)
        Right (bs, p') | tlNull bs -> go0 p'
        Right (bs, p') -> return $ Free (go1 (yield bs >> p'))

    go1 p = do
      p' <- _lineRn p
      return $ FreeT $ do
        next p' >>= \case
          Left r -> return (Pure r)
          Right (bs, p'') -> go0 (yield bs >> p'')

blah frags = map B.unpack $ P.toList (P.folds mappend mempty id $ _linesRn $ P.each frags)

test :: IO ()
test = do
  let br = "\r\n" :: B.ByteString
  let strs = ["\NAK>","\f\226\164z_j\r\n/\r","\ny\226d]-x\201\&6\254"] :: [B.ByteString]
  putStrLn $ "break " ++ show strs ++ " by " ++ show br
  putStrLn "==="
  rest <- runEffect $ for (_lineRn $ mapM_ yield strs) (lift . print)
  putStrLn "==="
  runEffect $ (for rest) (lift . print)
  putStrLn "==="


_lineRn :: (TextLike a, Monad m) => Producer a m r -> Producer a m (Producer a m r)
_lineRn = _breakBy "\r\n"

_breakBy :: (TextLike a, Monad m) => a -> Producer a m r -> Producer a m (Producer a m r)
_breakBy a p = lift (next p) >>= \case
    Left r -> return (return r)
    Right (bs, p') -> execStateT (breakByP a) (yield bs >> p')

-- Yield data from underlying producer before the delimiter, while stripping the delimeter out.
breakByP :: (TextLike a, Monad m) => a -> ParserP a m ()
breakByP str = go
  where
    strlen = tlLength str
    go = 
      drawP >>= \case
        Nothing -> return ()

        -- null str, skip to next chunk, bs is non null from here on out.
        Just bs | tlNull bs -> go
        Just bs -> case tlBreakSubstring str bs of
 
           -- null pref means delimiter must be in suff -> drop the delimeter from suff and yield nothing
           (pref, suff) | tlNull pref -> do
             when (tlLength suff > strlen) (unDrawP (tlDrop strlen suff))
 
           -- null suff means pref has no delimeter or partial delimiter and we need to fetch more chunks to be sure
           (pref, suff) | tlNull suff -> do
              stripped <- hoist lift (endsP str (tlLength bs - (tlLength str - 1)) bs)
              unless stripped go

           -- non null pref and suff means suff has delimeter with possible remainder
           (pref, suff) -> do
             yieldP pref
             when (tlLength suff > strlen) (unDrawP (tlDrop strlen suff))


-- if next Producer chunk trails with str, starting at offset n of that first chunk (for efficiency's sake),
-- drop str from stream, leave entire rest of stream untouched (if str was even there).
-- Needs to return stuff to yield AND whether it stripped a delimiter.
endsP :: (TextLike a, Monad m) => a -> Int -> a -> P.Parser a m Bool
endsP = \str n bs -> (go1 str n bs)
  where
    go0 str n = do
      draw >>= \case
        Nothing -> return False
        Just bs | tlNull bs -> go0 str n
        Just bs -> go1 str n bs

    go1 str n bs | tlNull bs = do
      go0 str n

    go1 str n bs | str `tlIsPrefixOf` tlDrop n bs = do
      unless (n + tlLength str == tlLength bs) (unDraw (tlDrop (tlLength str) (tlDrop n bs)))
      when (n > 0) (unDraw (tlTake n bs))
      return True

    go1 str n bs | tlDrop n bs `tlIsPrefixOf` str = do
      go0 (tlDrop (tlLength bs - n) str) 0 >>= \case
        True -> when (n > 0) (unDraw (tlTake n bs)) >> return True
        False -> unDraw bs >> return False

    go1 _ n bs | n > 0 && tlLength bs <= n = do
      unDraw bs
      return False

    go1 str n bs = do
      unDraw bs
      go0 str $! (n+1)
