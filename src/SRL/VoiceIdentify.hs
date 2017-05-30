module SRL.VoiceIdentify where

import           Data.Either             (lefts)
import           NLP.Type.PennTreebankII
  
  
ancestorTree :: PennTreeGen c p a -> PennTreeGen c p ([PennTreeGen c p a],a)
ancestorTree = go []
  where
    go xs x@(PN c ys) = PN c (map (go (x:xs)) ys)
    go xs x@(PL p y)  = PL p (xs,y)

ancestorTreeTagOnly :: PennTreeGen c p a -> PennTreeGen c p ([c],a)
ancestorTreeTagOnly = fmap (\(xs,y) -> (lefts (map getTag xs),y)) . ancestorTree

daughters :: PennTreeGen c p a -> [PennTreeGen c p a]
daughters (PN c xs) = xs
daughters (PL p x) = []

siblings :: PennTreeGen c p ([PennTreeGen c p a],a) -> PennTreeGen c p ([PennTreeGen c p a],a)
siblings = trimap id id f
  where
    f ([],x) = ([],x)
    f (y:ys,x) = (daughters y,x)


    
