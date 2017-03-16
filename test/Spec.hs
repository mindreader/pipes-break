{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Main (main) where


import Test.QuickCheck

import Control.Monad (void)

import Data.Tuple (swap)

import qualified Data.ByteString.Char8 as B

import Pipes as P
import qualified Pipes.Prelude as P

import Data.List (mapAccumL, isInfixOf)

import Pipes.Break.ByteString as PB

import Control.Monad.Identity

-- type BS = B.ByteString

-- debug :: (MonadIO m, Show a) => Producer a m (Producer a m r) -> m ()
-- debug p = do
--   (beg,rest) <- P.toListM' p
--   end <- P.toListM (void rest)
--      
--   liftIO $ putStrLn $ "beg:" <> show beg
--   liftIO $ putStrLn $ "rest:" <> show end
--   return ()

-- A string that most likely contains this delimiter or at least some parts of it in a few places
arbitraryStringWithDelimiter :: String -> Gen String
arbitraryStringWithDelimiter "" = arbitrary
arbitraryStringWithDelimiter del = do
  n <- choose (1,1000)
  mconcat <$> vectorOf n (oneof [return del, pieceOfDelim, someStr])
  where
    -- a long string where the delimiter is not contained in it
    someStr = arbitrary `suchThat` (\str -> not (del `isInfixOf` str))

    -- randomly pull out a peice of the delimiter
    pieceOfDelim = do
      beg <- choose (1,length del)
      end <- choose (0, beg - 1)
      return $ drop end $  take beg $ del

-- Split up a string into randomly sized chunks
arbitrarySplit :: String -> Gen [String]
arbitrarySplit bs = do
 offsets <- arbitrarySplits (length bs)
 let (rest, items) = mapAccumL (\str idx -> swap $ splitAt idx str) bs offsets
 return $ items ++ [rest]
 where
   arbitrarySplits :: Int -> Gen [Int]
   arbitrarySplits n = listOf (choose (0,10)) `suchThat` (\ls -> sum ls <= n)

-- For any delimiter, for any string that has pieces of that delimiter in it,
-- which has been broken up into random chunks, every call to breakBy should
-- at least be advancing the stream on each call.
prop_WillFinish :: String -> Property
prop_WillFinish delimiter =
  forAll (arbitraryStringWithDelimiter delimiter) $ \str ->
    forAll (arbitrarySplit str) $ \frags ->
      let remainder = runIdentity $ do
            rest <- runEffect $ breakBy (B.pack delimiter) (traverse (yield . B.pack) frags) >-> P.drain
            B.unpack . mconcat <$> P.toListM (void rest)
        
      in length str > 0 ==> length str > length remainder

-- Ensure invertibility of breaksBy and unBreaksBy
prop_BreaksEquiv :: String -> Property
prop_BreaksEquiv delimiter =
  forAll (arbitraryStringWithDelimiter delimiter) $ \str ->
    forAll (arbitrarySplit str) $ \frags ->
      str === B.unpack (breakByThenBackToStr (B.pack <$> frags) (B.pack delimiter))
  where
    breakByThenBackToStr :: [B.ByteString] -> B.ByteString -> B.ByteString
    breakByThenBackToStr frags br = mconcat $ P.toList (PB.unBreaksBy br $  PB.breaksBy br $ P.each frags)

-- Ensure invertibility of breakBy and unBreakBy
prop_BreakEquiv :: String -> Property
prop_BreakEquiv delimiter =
  forAll (arbitraryStringWithDelimiter delimiter) $ \str ->
    forAll (arbitrarySplit str) $ \frags ->
      str === (B.unpack $ mconcat $ P.toList $ unBreakBy (B.pack delimiter) $ breakBy (B.pack delimiter) $ P.each (B.pack <$> frags))


return []
runTests :: IO Bool
-- runTests = $verboseCheckAll
runTests = $quickCheckAll

main :: IO ()
main = void runTests
