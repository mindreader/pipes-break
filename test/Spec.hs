{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Main (main) where


import Test.QuickCheck

import Control.Monad (void)

import Data.Tuple (swap)

import qualified Data.ByteString.Char8 as B

import Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Group as P

import Data.List (mapAccumL, isInfixOf)

import Pipes.ByteString.Lines as PL

import Control.Monad.Identity
import Pipes.Lines.Internal

-- type BS = B.ByteString

-- debug :: (MonadIO m, Show a) => Producer a m (Producer a m r) -> m ()
-- debug p = do
--   (beg,rest) <- P.toListM' p
--   end <- P.toListM (void rest)
--      
--   liftIO $ putStrLn $ "beg:" <> show beg
--   liftIO $ putStrLn $ "rest:" <> show end
--   return ()

newtype LineyString = LineyString String deriving Show

instance Arbitrary LineyString where
  arbitrary = LineyString <$> lineyString

-- A string with a lot of newlines and carriage returns in it.
lineyString :: Gen String
lineyString = do
  n <- choose (1,100)
  concat <$> vectorOf n (frequency [(15, arbitrary), (10, return "\r"), (10, return "\n"), (1, return "\r\n")])

-- A string that most likely contains this delimiter or at least some parts of it in a few places
arbitraryStringWithDelimiter :: String -> Gen String
arbitraryStringWithDelimiter "" = arbitrary
arbitraryStringWithDelimiter del = do
  n <- choose (1,100)
  mconcat <$> vectorOf n (oneof [return del, pieceOfDelim, someStr])
  where
    -- a long string where the delimiter is not contained in it
    someStr = arbitrary `suchThat` (\str -> not (del `isInfixOf` str))

    -- randomly pull out a peice of the delimiter
    pieceOfDelim = do
      beg <- choose (1,length del)
      end <- choose (0, beg - 1)
      return $ drop end $  take beg $ del

-- For any delimiter, for any string that has pieces of that delimiter in it,
-- which has been broken up into random chunks, every call to _breakBy should
-- at least be advancing the stream on each call.
prop_WillFinish :: String -> Property
prop_WillFinish delimiter =
  forAll (arbitraryStringWithDelimiter delimiter) $ \str ->
    forAll (arbitrarySplit str) $ \frags ->
      let remainder = runIdentity $ do
            rest <- runEffect $ _breakBy (B.pack delimiter) (traverse (yield . B.pack) frags) >-> P.drain
            B.unpack . mconcat <$> P.toListM (void rest)
        
      in length str > 0 ==> length str > length remainder

-- For any delimiter, for any string that has pieces of that delimiter in it,
-- which has been broken up into random chunks, grouped by delimiter, folded
-- each producer back into a bytestring with the delimiter readded, the result
-- should be exactly the same as the original sentence.
prop_SplitTest :: String -> Property
prop_SplitTest delimiter =
  forAll (arbitraryStringWithDelimiter delimiter) $ \str ->
    forAll (arbitrarySplit str) $ \frags ->
      str === B.unpack (breakByThenBackToStr (B.pack <$> frags) (B.pack delimiter))
  where
    breakByThenBackToStr :: [B.ByteString] -> B.ByteString -> B.ByteString
    breakByThenBackToStr frags br = B.intercalate br $ P.toList (P.folds mappend mempty id $ PL.breaksBy br $ P.each frags)

arbitrarySplit :: String -> Gen [String]
arbitrarySplit bs = do
 offsets <- arbitrarySplits (length bs)
 let (rest, items) = mapAccumL (\str idx -> swap $ splitAt idx str) bs offsets
 return $ items ++ [rest]
 where
   arbitrarySplits :: Int -> Gen [Int]
   arbitrarySplits n = listOf (choose (0,10)) `suchThat` (\ls -> sum ls <= n)

return []
runTests :: IO Bool
-- runTests = $verboseCheckAll
runTests = $quickCheckAll

main :: IO ()
main = void runTests
