{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.HUnit
import Test.Framework hiding (Test)
import Test.Framework.Providers.HUnit hiding (Test)

tests = hUnitTestToTests $ TestList [testSafeHeadForNonEmptyList, testSafeHeadForEmptyList]

main = defaultMain tests

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

testSafeHeadForNonEmptyList :: Test
testSafeHeadForNonEmptyList =
    TestCase $ assertEqual "Should return (Just head) for non empty list" (Just 2)
               (safeHead ([1]::[Int]))

testSafeHeadForEmptyList :: Test
testSafeHeadForEmptyList = 
    TestCase $ assertEqual "Should return Nothing for empty list"
                           Nothing (safeHead ([]::[Int]))
