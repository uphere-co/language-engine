module SRL.VoiceIdentify where

import           NLP.Type.PennTreebankII
  
ancestorTreeTagOnly :: PennTreeGen c p a -> PennTreeGen c p ([c],a)
ancestorTreeTagOnly = go []
  where
    go cs (PN c xs) = PN c (map (go (c:cs)) xs)
    go cs (PL p x)  = PL p (cs,x)

ancestorTree :: PennTreeGen c p a -> PennTreeGen c p ([PennTreeGen c p a],a)
ancestorTree = go []
  where
    go xs x@(PN c ys) = PN c (map (go (x:xs)) ys)
    go xs x@(PL p y)  = PL p (xs,y)

daughters :: PennTreeGen c p a -> [PennTreeGen c p a]
daughters (PN c xs) = xs
daughters (PL p x) = []

siblings :: PennTreeGen c p ([PennTreeGen c p a],a) -> PennTreeGen c p ([PennTreeGen c p a],a)
siblings = trimap id id f
  where
    f ([],x) = ([],x)
    f (y:ys,x) = (daughters y,x) -- ([y],x) -- (daughters y,x)
    
