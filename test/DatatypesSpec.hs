module DatatypesSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck

import Datatypes

spec :: Spec
spec = do
    describe "implies'" $ do
        it "behaves like implies" $ property prop_implies'_implies

prop_implies'_implies :: Bool -> Bool -> Property
prop_implies'_implies x y =
    implies' x y === implies x y
