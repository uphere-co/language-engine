{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module SRL.Format where

import           Control.Lens
import qualified Data.IntMap             as IM
import           Data.Maybe
import           Data.Monoid
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
import           SRL.Type.Clause
import           SRL.Type.Verb
import           SRL.Util



formatRngText :: [Text] -> (Int,Int) -> String
formatRngText terms p = show p ++ ": " ++ T.unpack (clippedText p terms)


formatBitree :: (a -> Text) ->  Bitree a a -> Text
formatBitree fmt tr = linePrint fmt (toTree (bimap id id tr))
  where -- f (_,l,_) =  l
        toTree (PN x xs) = Tr.Node x (map toTree xs)
        toTree (PL x)    = Tr.Node x []
        

formatVerbProperty :: VerbProperty -> String
formatVerbProperty vp = printf "%3d %15s %8s %20s %8s %s"
                          (vp^.vp_index) (vp^.vp_lemma.to unLemma)
                          (show (vp^.vp_tense)) (show (vp^.vp_aspect)) (show (vp^.vp_voice))
                          (show (vp^.vp_words))


formatVerbArgs :: (Show b) => VerbArgs (Either STag b) -> String
formatVerbArgs va = printf "%10s %-25s"
                      (formatArg (va^.va_arg0))
                      (T.intercalate " " (map (^._2) (va^.va_string)))
  where
    formatArg = maybe "" $ \a ->
                  case a of
                    Right p -> show p
                    Left (S_RT)      -> "ROOT"
                    Left (S_SBAR _)  -> "SBAR"
                    Left (S_CL c)    -> show c
                    Left (S_VP _)    -> "VP"
                    Left (S_PP t)    -> "(PP " ++ show t ++ ")"
                    Left (S_OTHER t) -> show t
                    -- show -- id --  \a -> printf "%s" (show a)
    

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


