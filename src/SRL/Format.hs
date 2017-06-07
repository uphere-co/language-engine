module SRL.Format where

import           Control.Lens
import qualified Data.Text               as T
import           Text.Printf
--
import           NLP.Type.PennTreebankII
import           PropBank.Type.Prop
--
import           SRL.Type

{- 
formatVoice :: Voice -> String
formatVoice Nothing = " "
formatVoice (Just Active) = "active"
formatVoice (Just Passive) = "passive"
-}

formatPTP :: ParseTreePath -> String
formatPTP = foldMap f 
  where
    f (Left  c,Up  ) = show c ++ "↑"
    f (Left  c,Down) = show c ++ "↓"
    f (Right p,Up  ) = show p ++ "↑"
    f (Right p,Down) = show p ++ "↓"


formatArgNodeFeature :: Int -> ArgNodeFeature -> String
formatArgNodeFeature predidx (label,(rng,ptp,mhead)) =
    printf "%10s %10s %6s %30s %5s %s" (T.unpack (pbLabelText label)) (show rng) (show (position predidx rng))
                                       (formatPTP ptp) (w^._1) (w^._2)
  where
    w = hstr mhead
    hstr Nothing = ("","")
    hstr (Just (_,(_,(p,w)))) = (show p, T.unpack w)


formatInstanceFeature :: InstanceFeature -> String
formatInstanceFeature (predidx,(lemma,sensenum),voicefeature,argfeatures) =
  let fs = concat argfeatures
  in flip foldMap fs $ \x -> printf "%3s %17s.%2s %7s %s\n"
                              (show predidx)
                              lemma sensenum
                              (show voicefeature)
                              (formatArgNodeFeature predidx x)


