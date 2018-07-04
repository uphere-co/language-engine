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
import           Formatting                              (Format,(%),(%.),sformat,stext,int)
import qualified Formatting                      as F    (left,right)
--
import           Data.ListZipper               (ListZipper(..))
import           NLP.Type.PennTreebankII
import           PropBank.Type.Prop
--
import           SRL.Old.Type
import           SRL.Old.Util


-- note that left/right are reversed
rs,ls :: Int -> Format r (Text -> r)
rs n = F.left  n ' ' %. stext
ls n = F.right n ' ' %. stext

rd,ld :: (Integral a) => Int -> Format r (a -> r)
rd n = F.left  n ' ' %. int
ld n = F.right n ' ' %. int


formatRngText :: [Text] -> (Int,Int) -> Text
formatRngText terms p = T.pack (show p) <> ": " <> clippedText p terms


showVerb :: IntMap Text -> (Lemma ,[Int]) -> Text
showVerb tkmap (lma,is) = unLemma lma <> " : " <> fullwords
  where fullwords = T.intercalate " " $ map (\i -> fromMaybe "" (IM.lookup i tkmap)) is


formatPTP :: ParseTreePath -> Text
formatPTP = foldMap f 
  where
    f (Left  c,Up  ) = T.pack (show c) <> "↑"
    f (Left  c,Down) = T.pack (show c) <> "↓"
    f (Right p,Up  ) = T.pack (show p) <> "↑"
    f (Right p,Down) = T.pack (show p) <> "↓"


formatDRP :: ListZipper DepInfo -> Text
formatDRP (LZ tostart rootnode totarget) = foldMap f lst
  where
    lst1 = (view dinfo_rel rootnode,Down):map ((,Down).view dinfo_rel) totarget
    lst2 = map ((,Up). view dinfo_rel) . reverse $ tostart
    lst = lst2 ++ lst1
    f (r,Up  ) = T.pack (show r) <> "↑"
    f (r,Down) = T.pack (show r) <> "↓"


formatArgNodeFeature :: Int -> ArgNodeFeature -> Text
formatArgNodeFeature predidx (AFeat label (SRLFeat rng ptp mdrp mhead)) =
    sformat (rs 10 % " " % rs 10 " " % rs 6 % " " % rs 50 % " " % rs 50 % " " % rs 5 % " " % stext)
      (T.unpack (pbLabelText label)) (show rng) (show (position predidx rng))
      (formatPTP ptp)
      (maybe "" formatDRP mdrp)
      (w^._1)
      (w^._2)
  where
    w = hstr mhead
    hstr Nothing = ("","")
    hstr (Just (_,(_,(p,t)))) = (show p, T.unpack t)


formatInstanceFeature :: InstanceFeature -> Text
formatInstanceFeature (IFeat predidx (lemma,sensenum) voicefeature argfeatures) =
  let fs = concat argfeatures
  in flip foldMap fs $ \x -> sformat (rs 3 % " " % rs 17 % "." % rs 2" "% rs 7 %" "% stext % "\n")
                               (T.pack (show predidx))
                               lemma
                               sensenum
                               (T.pack (show voicefeature))
                               (formatArgNodeFeature predidx x)


