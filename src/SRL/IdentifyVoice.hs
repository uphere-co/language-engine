{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SRL.IdentifyVoice where

 
import           Control.Lens
import           Control.Monad                 ((<=<))
import           Data.Either                   (lefts)
import qualified Data.IntMap             as IM
import           Data.Maybe                    (fromJust)
import           Data.Text                     (Text)
import qualified Data.Text               as T
--
import           NLP.Type.PennTreebankII
import           NLP.Type.TreeZipper
--
import           SRL.Util


lemmatize :: IM.IntMap Text
          -> PennTreeIdxG ChunkTag (POSTag,Text)
          -> PennTreeIdxG ChunkTag (POSTag,(Text,Text))
lemmatize m = bimap id (\(i,(p,x)) -> (i,(p,(x,fromJust (IM.lookup i m)))))


type TreeICP a = Tree (Range,ChunkTag) (Int,(POSTag,a))

type TreeZipperICP a = TreeZipper (Range,ChunkTag) (Int,(POSTag,a))

isVBN :: TreeZipperICP a -> Bool
isVBN z = case current z of
            PL (_,(p,_)) -> p == VBN
            _            -> False 

withCopula :: TreeZipperICP (Text,Text) -> Bool
withCopula z = case current <$> (prev <=< parent) z of
                 Just (PL (_,(_,(_,l)))) -> l == "be"
                 _                       -> False

isInNP :: TreeZipperICP (Text,Text) -> Bool
isInNP z = case current <$> (parent <=< parent) z of
             Just (PN (_,c) _) -> c == NP
             _             -> False


isInPP :: TreeZipperICP (Text,Text) -> Bool
isInPP z = case current <$> (parent z) of
             Just (PN (_,c) _) -> c == PP
             _                 -> False

-- rule3 z :: TreeZipperICP (Text,
