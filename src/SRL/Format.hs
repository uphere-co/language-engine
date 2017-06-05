module SRL.Format where

import           Control.Lens
import qualified Data.Text               as T
import           Text.Printf
--
import           NLP.Type.PennTreebankII
--
import           SRL.Type


formatVoice :: Maybe Voice -> String
formatVoice Nothing = " "
formatVoice (Just Active) = "active"
formatVoice (Just Passive) = "passive"

formatPTP :: ParseTreePath -> String
formatPTP = foldMap f 
  where
    f (Left  c,Up  ) = show c ++ "↑"
    f (Left  c,Down) = show c ++ "↓"
    f (Right p,Up  ) = show p ++ "↑"
    f (Right p,Down) = show p ++ "↓"


formatArgNodeFeature :: Int -> ArgNodeFeature -> String
formatArgNodeFeature predidx (label,(rng,ptp,mhead)) =
    printf "%10s %10s %6s %30s %5s %s" (T.unpack label) (show rng) (show (position predidx rng))
                                       (formatPTP ptp) (w^._1) (w^._2)
  where
    w = hstr mhead
    hstr Nothing = ("","")
    hstr (Just (_,(_,(p,w)))) = (show p, T.unpack w)


formatInstanceFeature :: InstanceFeature -> String
formatInstanceFeature (predidx,rolesetid,voicefeature,argfeatures) =
  let fs = concat argfeatures
  in foldMap (\x -> printf "%3s %20s %7s %s\n" (show predidx) (T.unpack rolesetid) (formatVoice voicefeature) (formatArgNodeFeature predidx x)) fs


