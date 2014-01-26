module Tests (tests) where

import Data.Char
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as C
import Data.RFC1751
import Test.Framework
import Test.Framework.Providers.QuickCheck2

tests :: [Test]
tests =
    [ testGroup "RFC-1751"
        [ testProperty "Encode/Decode RFC-1751 keys" decodeEncode
        , testProperty "Encode/Decode RFC-1751 keys (lowercase)" decodeEncodeLc
        , testProperty "Double 64-bit key" doubleWord64
        ]
    ]

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Either.fromRight: Left"

decodeEncode :: (Word64, Word64) -> Bool
decodeEncode (w1, w2) =
   bs == bs'
 where
   bs = encode w1 `C.append` encode w2
   hk = decode bs :: HumanKey
   bs' = encode hk

decodeEncodeLc :: (Word64, Word64) -> Bool
decodeEncodeLc (w1, w2) =
    bs == bs'
  where
    bs = encode w1 `C.append` encode w2
    hk = decode bs :: HumanKey
    hk' = fromRight . humanKey . map toLower $ show hk
    bs' = encode hk'

doubleWord64 :: Word64 -> Bool
doubleWord64 w =
    ws1 == ws2
  where
    bs = encode w `C.append` encode w
    hk = decode bs :: HumanKey
    (ws1, ws2) = splitAt 6 $ words $ show hk
