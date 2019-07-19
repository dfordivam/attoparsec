module QC.Text.FastSet where

import QC.Common (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec.Text.FastSet as FastSet

membershipCorrect :: String -> String -> Property
membershipCorrect members others =
    let fs = FastSet.fromList members
        correct c = (c `FastSet.member` fs) == (c `elem` members)
    in property $ all correct (members ++ others)

tests :: IO ()
tests = testProperty "membership is correct" membershipCorrect
