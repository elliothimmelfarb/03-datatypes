{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module TablesSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck

import Tables

spec :: Spec
spec = do
    describe "Table" $ do
        it "deleting a pair after inserting it into the empty table gives the empty table" $ property prop_delete_empty

deriving newtype instance (Eq k, Eq v) => Eq (Table k v)

prop_delete_empty :: Int -> Char -> Property
prop_delete_empty n c = delete n (insert n c empty) === empty
