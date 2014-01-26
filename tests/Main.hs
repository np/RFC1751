module Main where

import Test.Framework (defaultMain)

-- Property testing
import qualified Tests (tests)

-- Unit testing
import qualified Units (tests)

main :: IO ()
main = defaultMain (Tests.tests ++ Units.tests)
