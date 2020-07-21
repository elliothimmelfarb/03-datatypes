{-# OPTIONS_GHC -Wno-orphans #-}

module TransactionsSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck

import Transactions

spec :: Spec
spec = do
    describe "flipTransaction" $ do
        it "is an involution" $ property prop_flip_flip

instance Arbitrary Transaction where
    arbitrary = Transaction <$> arbitrary <*> arbitrary <*> arbitrary
    shrink (Transaction a f t) =
        [Transaction a' f t | a' <- shrink a] ++
        [Transaction a f' t | f' <- shrink f] ++
        [Transaction a f t' | t' <- shrink t]

prop_flip_flip :: Transaction -> Property
prop_flip_flip t = flipTransaction (flipTransaction t) === t
