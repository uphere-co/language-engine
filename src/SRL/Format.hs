{-# LANGUAGE TupleSections #-}

module SRL.Format where

import           Control.Lens
import qualified Data.Text               as T
import           Data.Text                    (Text)
import           Data.Tree               as Tr
import           Text.Printf
--
import           Data.Attribute
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
import           PropBank.Type.Prop
import           Text.Format.Tree
--
import           SRL.Type
import           SRL.Util

{- 
formatVoice :: Voice -> String
formatVoice Nothing = " "
formatVoice (Just Active) = "active"
formatVoice (Just Passive) = "passive"
-}

formatRngText :: [Text] -> (Int,Int) -> String
formatRngText terms p = show p ++ ": " ++ T.unpack (clippedText p terms)


formatTree :: Bitree (Int,Lemma,a) (Int,Lemma,a) -> Text
formatTree tr = linePrint unLemma (toTree (bimap (^._2) (^._2) tr))
  where f (_,l,_) =  l
        toTree (PN x xs) = Tr.Node x (map toTree xs)
        toTree (PL x)    = Tr.Node x []
        

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


