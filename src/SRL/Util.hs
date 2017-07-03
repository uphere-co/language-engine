{-# LANGUAGE OverloadedStrings #-}

module SRL.Util where

import           Data.Text      (Text)
import qualified Data.Text as T (intercalate, unpack)
--
import           NLP.Type.PennTreebankII


clippedText :: (Int,Int) -> [Text] -> Text
clippedText (b,e) = T.intercalate " " . drop b . take (e+1) 


formatRngText :: [Text] -> (Int,Int) -> String
formatRngText terms p = show p ++ ": " ++ T.unpack (clippedText p terms)


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


extractIndexOut :: PennTreeGen (i,c) (j,t) -> PennTreeGen c t
extractIndexOut (PN (_,c) xs) = PN c (map extractIndexOut xs)
extractIndexOut (PL (_,x)) = PL x 

