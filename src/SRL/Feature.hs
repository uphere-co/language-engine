{-# LANGUAGE MultiWayIf #-}

module SRL.Feature where

import           Data.Text (Text)
--
import           NLP.Type.PennTreebankII
--
import           SRL.Util


data Position = Before | After | Embed 

phraseType :: PennTreeGen ChunkTag POSTag a -> Either ChunkTag POSTag
phraseType (PN c _) = Left c
phraseType (PL t _) = Right t

position :: Int ->  PennTreeGen c p (Int,a) -> Position
position n tr = let (b,e) = termRange tr
                in if | n < b     -> Before
                      | n > e     -> After
                      | otherwise -> Embed

parseTreePath :: (Int,Range) -> PennTreeGen (Range,c) (Range,p) (Int,a) -> Maybe ([c],[c])
parseTreePath (start,target) tr =
          if start `isInside` (b,e) && target `isInsideR` (b,e)
             then Just ([],[])
             else Nothing
  where (b,e) = termRange tr


