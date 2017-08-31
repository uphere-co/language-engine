{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module SRL.Old.Format where

import           Control.Lens
import           Data.IntMap                   (IntMap)
import qualified Data.IntMap             as IM
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text               as T
import           Data.Text                     (Text)
import           Text.Printf            hiding (formatArg)
--
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
import           PropBank.Type.Prop
--
import           SRL.Old.Type
import           SRL.Old.Util



formatRngText :: [Text] -> (Int,Int) -> String
formatRngText terms p = show p ++ ": " ++ T.unpack (clippedText p terms)


showVerb :: IntMap Text -> (Lemma ,[Int]) -> Text
showVerb tkmap (lma,is) = unLemma lma <> " : " <> fullwords
  where fullwords = T.intercalate " " $ map (\i -> fromMaybe "" (IM.lookup i tkmap)) is


formatPTP :: ParseTreePath -> String
formatPTP = foldMap f 
  where
    f (Left  c,Up  ) = show c ++ "↑"
    f (Left  c,Down) = show c ++ "↓"
    f (Right p,Up  ) = show p ++ "↑"
    f (Right p,Down) = show p ++ "↓"


formatDRP :: ListZipper DepInfo -> String
formatDRP (LZ tostart root totarget) = foldMap f lst
  where
    lst1 = (view dinfo_rel root,Down):map ((,Down).view dinfo_rel) totarget
    lst2 = map ((,Up). view dinfo_rel) . reverse $ tostart
    lst = lst2 ++ lst1
    f (r,Up  ) = show r ++ "↑"
    f (r,Down) = show r ++ "↓"


formatArgNodeFeature :: Int -> ArgNodeFeature -> String
formatArgNodeFeature predidx (AFeat label (SRLFeat rng ptp mdrp mhead)) =
    printf "%10s %10s %6s %50s %50s %5s %s" (T.unpack (pbLabelText label)) (show rng) (show (position predidx rng))
                                            (formatPTP ptp) (maybe "" formatDRP mdrp)
                                            (w^._1) (w^._2)
  where
    w = hstr mhead
    hstr Nothing = ("","")
    hstr (Just (_,(_,(p,t)))) = (show p, T.unpack t)


formatInstanceFeature :: InstanceFeature -> String
formatInstanceFeature (IFeat predidx (lemma,sensenum) voicefeature argfeatures) =
  let fs = concat argfeatures
  in flip foldMap fs $ \x -> printf "%3s %17s.%2s %7s %s\n"
                              (show predidx)
                              lemma sensenum
                              (show voicefeature)
                              (formatArgNodeFeature predidx x)


