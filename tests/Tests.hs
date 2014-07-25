module Tests (tests) where

import Control.Applicative
import Data.Char
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe
import Data.RFC1751
import Test.Framework
import Test.Framework.Providers.QuickCheck2

tests :: [Test]
tests =
    [ testGroup "RFC-1751"
        [ testProperty "Encode/Decode RFC-1751 keys" decodeEncode
        , testProperty "Encode/Decode RFC-1751 keys (lowercase)" decodeEncodeLC
        , testProperty "Double 64-bit key" doubleWord64
        ]
    ]

decodeEncode :: (Word64, Word64) -> Bool
decodeEncode (w1, w2) = fromMaybe False $
   (== bs) <$> (mnemonicToKey =<< keyToMnemonic bs)
 where
   bs = encode w1 `L8.append` encode w2

decodeEncodeLC :: (Word64, Word64) -> Bool
decodeEncodeLC (w1, w2) = fromMaybe False $
    (== bs) <$> (mnemonicToKey =<< map toLower <$> keyToMnemonic bs)
  where
    bs = encode w1 `L8.append` encode w2
    

doubleWord64 :: Word64 -> Bool
doubleWord64 w = fromMaybe False $ do
    (\(x, y) -> x == y) . splitAt 6 . words <$> keyToMnemonic bs
  where
    bs = encode w `L8.append` encode w
