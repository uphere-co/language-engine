--
-- This is a module that collects operation and comparison on range
--
module Data.Range where

type Range = (Int,Int)

isInside :: Int -> Range -> Bool
x `isInside` (x1,y1) = x1 <= x && x <= y1


isInsideR :: Range -> Range -> Bool
(x0,y0) `isInsideR` (x1,y1) = x1 <= x0 && y0 <= y1
