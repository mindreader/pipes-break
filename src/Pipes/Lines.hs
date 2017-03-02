{-# LANGUAGE LambdaCase, OverloadedStrings, PartialTypeSignatures, NoMonomorphismRestriction, DeriveGeneric, Rank2Types #-}

module Pipes.Lines (
  _unLinesRn, _linesRn, _lineRn
) where

import Data.String (IsString)

import Pipes as P
import Pipes.Group as P

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import Control.Monad (when, (>=>))


class (Eq a, IsString a) => TextLike a where
  tlNull :: a -> Bool
  tlBreakSubstring :: a -> a -> (a, a)
  tlLength :: a -> Int
  tlTake :: Int -> a -> a
  tlDrop :: Int -> a -> a
  tlHead :: a -> Char
  tlLast :: a -> Char
  tlIsPrefixOf :: a -> a -> Bool

instance TextLike B.ByteString where
  tlNull = B.null
  tlBreakSubstring = B.breakSubstring
  tlLength = B.length
  tlTake = B.take
  tlDrop = B.drop
  tlHead = B.head
  tlLast = B.last
  tlIsPrefixOf = B.isPrefixOf

instance TextLike T.Text where
  tlNull = T.null
  tlBreakSubstring = T.breakOn
  tlLength = T.length
  tlTake = T.take
  tlDrop = T.drop
  tlHead = T.head
  tlLast = T.last
  tlIsPrefixOf = T.isPrefixOf


_unLinesRn ::(TextLike a, Monad m) => FreeT (Producer a m) m r -> Producer a m r
_unLinesRn = concats . maps (<* yield "\r\n")

-- | Group a producer of bytestrings into a series of producers delimited by "\r\n"
--   The \r\n are dropped.
_linesRn :: (TextLike a, Monad m) => Producer a m r -> FreeT (Producer a m) m r
_linesRn p = FreeT $ do
  next p >>= return . \case
    Left r       -> Pure r
    Right (bs, p') -> Free (go1 (yield bs >> p'))

  where
    go1 = _lineRn >=> return . _linesRn


_lineRn :: (TextLike a, Monad m) => Producer a m r -> Producer a m (Producer a m r)
_lineRn p = do
  nextNonNull p >>= \case
    Left r -> return (return r)
    Right ("\r\n", p') -> return p'
    Right (bs, p') -> do
      case tlBreakSubstring "\r\n" bs of
        (pref,suff) | tlNull pref && tlIsPrefixOf "\r\n" suff -> return (yield (tlDrop 2 suff) >> p')
        (pref,suff) | tlNull pref -> return (yield suff >> p')
        (pref,suff) | suff == "\r\n" -> yield pref >> return p'
        (pref,suff) | tlIsPrefixOf "\r\n" suff -> yield pref >> return (yield (tlDrop 2 suff) >> p')

        (pref,_) | tlLast pref == '\r' -> nextNonNull p' >>= \case
          Left r -> yield pref >> return (return r)
          Right (bs', p'') | tlNull bs' -> yield "\r" >> return p''
          Right (bs', p'') | tlHead bs' == '\n' ->
            yield (tlTake ((tlLength pref) - 1) pref) >> return (when (tlLength bs' > 1) (yield $ tlDrop 1 bs') >> p'')
          Right (bs', p'') -> yield bs >> _lineRn (yield bs' >> p'')

        (pref, suff) | tlNull suff -> nextNonNull p' >>= \case
          Left r -> yield pref >> return (return r)
          Right (bs', p'') | tlIsPrefixOf "\r\n" bs' -> yield bs >> return (when (tlLength bs' > 2) (yield (tlDrop 2 bs')) >> p'')

          Right (bs', p'') | tlLength bs' == 1 && tlHead bs' == '\r' -> nextNonNull p'' >>= \case
            Left r -> yield bs >> yield bs' >> return (return r)
            Right (bs'', p''') | tlHead bs'' == '\n'-> yield bs >> return (when (tlLength bs'' > 1) (yield (tlDrop 1 bs'')) >> p''')
            Right (bs'', p''') -> yield bs >> yield "\r" >> _lineRn (yield bs'' >> p''')

          Right (bs', p'') -> yield bs >> _lineRn (yield bs' >> p'')

        (pref,suff) -> yield pref >> return (yield (tlDrop 2 suff) >> p')

  where
    dropNulls :: (TextLike a, Monad m) => Producer a m r -> Producer a m (Producer a m r)
    dropNulls = lift . next >=> \case
      Left r -> return (return r)
      Right (bs,p') | tlNull bs -> dropNulls p'
      Right (bs,p') -> return (yield bs >> p')

    nextNonNull :: (TextLike a, Monad m) => Producer a m r -> Producer a m (Either r (a, Producer a m r))
    nextNonNull = dropNulls >=> lift . next
