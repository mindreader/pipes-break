{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Lens (view, over)

import Control.Monad (replicateM, void)
import Control.Monad.Identity

import Data.Tuple (swap)

import Data.Monoid

import qualified Data.ByteString.Char8 as B

import Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Group as P

import Data.List (mapAccumL, isSubsequenceOf)

import Pipes.ByteString.Lines as PL

-- | Make up some words, combine them into a network stream, break network stream up randomly into chunks,
--   parse them back into words, ensure final words matches initial words
prop_splitrn :: SomeWords -> Property
prop_splitrn (SomeWords wds) = monadic runIdentity $ do
   frags <- pick $ splitString sentence
   let res = map B.unpack $ P.toList (P.folds mappend mempty id $ view PL.lines $ P.each frags)
   -- monitor $ collect (show $ length frags)
   assert (wds == res)
   where
     sentence = B.intercalate "\r\n" (map B.pack wds) <> "\r\n"

-- | Ensure identity property.
prop_ident :: SomeNetworkString -> Property
prop_ident (SomeNetworkString str) = monadic runIdentity $ do
  let prod = P.yield str :: Producer B.ByteString Identity ()
  assert (mconcat (P.toList $ over PL.lines id prod) == mconcat (P.toList prod))

-- | Ensure identity property even if stream is split up randomly into chunks.
prop_ident_split :: SomeNetworkString -> Property
prop_ident_split (SomeNetworkString str) = monadic runIdentity $ do
  str' <- pick $ splitString str
  assert (mconcat (P.toList $ over PL.lines id (P.each str')) == mconcat (P.toList (P.yield str)))

newtype SomeNetworkString = SomeNetworkString B.ByteString deriving Show

instance Arbitrary SomeNetworkString where
  arbitrary = SomeNetworkString . B.pack <$> arbitraryNetworkString
  shrink (SomeNetworkString "") = []
  shrink (SomeNetworkString bs) = [SomeNetworkString (B.drop 1 bs)]

arbitraryNetworkString :: Gen String
arbitraryNetworkString = do
  n <- choose (1,1500)
  (<> "\r\n") . concat <$> vectorOf n (frequency [(15, arbitrary), (1, return "\r\n")])

newtype SomeWords = SomeWords [String] deriving Show

instance Arbitrary SomeWords where
  arbitrary = arbitrarySomeWords
  shrink (SomeWords []) = []
  shrink (SomeWords [word]) = [SomeWords [(tail word)], SomeWords [(take (length word - 1) word)]]
  shrink (SomeWords wds) = [SomeWords (take (length wds - 1) wds), SomeWords (tail wds)]

arbitrarySomeWords :: Gen SomeWords
arbitrarySomeWords = do
    numWords <- choose (1,100)
    SomeWords <$> replicateM numWords arbitraryWord
  where
    arbitraryWord :: Gen String
    arbitraryWord = do
        len <- choose (0,300)
        str <- vector len `suchThat` (not . isSubsequenceOf "\r\n")
        return str

splitString :: B.ByteString -> Gen [B.ByteString]
splitString bs = do
 offsets <- arbitrarySplits (B.length bs - 1)
 let (rest, items) = mapAccumL (\str idx -> swap $ B.splitAt idx str) bs offsets
 return $ items ++ [rest]
 where
   arbitrarySplits :: Int -> Gen [Int]
   arbitrarySplits n = listOf (choose (1,10)) `suchThat` (\ls -> sum ls <= n)


return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = void runTests
