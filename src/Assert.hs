module Assert where

import Test.Tasty.HUnit                (Assertion, assertBool,assertEqual)

assert :: Bool -> Assertion
assert = assertBool ""

massertEqual :: (Eq a) => IO a -> a -> IO ()
massertEqual expr val = do
  r <- expr
  assert (r == val)

eassertEqual :: (Eq a, Show a) => a -> a -> Assertion
eassertEqual = assertEqual ""
