{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module NLP.Syntax.Argument where

import           Control.Lens                 ((^..),(^.),(^?),_1,_2,_Just,_Right,to)
import           Control.Monad                (join)
import           Data.Bifunctor               (bimap)
import           Data.Foldable                (toList)
import           Data.List                    (find)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>),Last(..))
import           Data.Text                    (Text)
import qualified Data.Text               as T
--
import           Data.Bitree                  (Bitree(..),getLeaves,getNodes,getRoot,_PL,_PN)
import           Data.BitreeZipper            (BitreeZipper(..),child1,current,next,parent
                                              ,mkBitreeZipper)
import           Data.BitreeZipper.Util       (firstSiblingBy)
import           NLP.Type.PennTreebankII      (PennTreeIdx,Range,POSTag(..),ChunkTag(..)
                                              ,LinkID,TernaryLogic(..)
                                              ,getRange
                                              ,identifyTrace
                                              ,isAdverb,isNoun
                                              )
import           NLP.Type.SyntaxProperty      (Voice(..))
import           PropBank.Match               (findNode)
import           PropBank.Type.Prop           (Argument(..),PropBankLabel(..)
                                              ,arg_label,arg_terminals)
--
import           Lexicon.Type
--
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar


headPreposition :: BitreeZipper (Range,ChunkTag) (Int,(POSTag,Text)) -> Maybe (Text,Bool)
headPreposition z = do
    p <- child1 z >>= firstSiblingBy next (\x -> let y = x^?_PL._2._1 in (y == Just IN) || (y == Just TO))
    t <- (current p)^? _PL . _2 . _2
    let b = fromMaybe False (fmap (== S) . (^?_PN._1._2) . current =<< next p)
    return (T.toLower t,b)


headAdverb :: [PennTreeIdx] -> Maybe Text
headAdverb xs = getLast (foldMap (Last . f) xs)
  where f (PN _ _)         = Nothing
        f (PL (_,(pos,t))) = if isAdverb pos then Just (T.toLower t) else Nothing


phraseNodeType :: Maybe (TP as) -> BitreeZipper (Range,ChunkTag) (Int,(POSTag,Text)) -> GRel
phraseNodeType mtp z
  = let rng = getRange (current z)
        subj = do tp <- mtp
                  dp <- tp^?specifier.trResolved._Just._Right   -- for the time being, later we will support CP subject
                  return (dp^.maximalProjection.to current.to getRange == rng)
        obj  = do tp <- mtp
                  -- The following must be changed to accommodate PP
                  let os = zip [1..] (tp^..complement.complement.traverse.trResolved.to (fmap compVPToEither))
                  m <- flip find os $ \o ->
                         case o^._2 of
                           Just (Right x) -> x^.maximalProjection.to current.to getRange == rng
                           Just (Left x)  -> (getRange . current) x == rng
                              -- x^?maximalProjection._Just.to current.to getRange == Just rng
                           Nothing        -> False
                  return (m^._1)
        mgarg :: Maybe GArg
        mgarg = case subj of
                  Just True -> Just GASBJ
                  _ -> case obj :: Maybe Int of
                         Just 1 -> Just GA1
                         Just 2 -> Just GA2
                         _ -> Nothing

        phrase = case current z of
                   PN (_,c) xs       -> case c of
                                          PP   -> GR_PP (headPreposition z)
                                          ADVP -> case headAdverb xs of
                                                    Just t -> GR_ADVP (Just t)
                                                    Nothing -> case fst <$> headPreposition z of
                                                                 Just t -> GR_ADVP (Just t)
                                                                 Nothing -> GR_X "??ADVP"
                                          PRT  -> GR_PP ((,False) <$> headAdverb xs)
                                          WHNP -> GR_NP mgarg
                                          NP   -> GR_NP mgarg
                                          S    -> GR_S  mgarg
                                          SBAR -> GR_SBAR mgarg
                                          _    -> GR_X (T.pack (show c))
                   PL (_,(D_NONE,t)) -> case parent z of
                                          Nothing -> GR_X ("??"<> t)
                                          Just z' -> phraseNodeType mtp z'
                   PL (_,(p     ,t)) -> case isNoun p of
                                          Yes -> GR_NP mgarg
                                          _   -> GR_X ("??" <> T.pack (show (p,t)))
    in phrase


zipperArgTable :: PennTreeIdx
               -> ArgTable (ATNode (Either Range Int))
               -> ArgTable (ATNode (BitreeZipper (Range,ChunkTag) (Int,(POSTag,Text))))
zipperArgTable itr tbl = tbl { _tbl_arg0 = replacef (_tbl_arg0 tbl)
                             , _tbl_arg1 = replacef (_tbl_arg1 tbl)
                             , _tbl_arg2 = replacef (_tbl_arg2 tbl)
                             , _tbl_arg3 = replacef (_tbl_arg3 tbl)
                             , _tbl_arg4 = replacef (_tbl_arg4 tbl)
                             }
  where
        zpr = mkBitreeZipper [] itr
        leaves = getLeaves zpr
        nodes = getNodes zpr
        findf :: Either Range Int -> Maybe (BitreeZipper (Range,ChunkTag) (Int,(POSTag,Text)))
        findf (Left rng) = find (\z -> case getRoot (current z) of Left (rng',_) -> rng==rng' ; _ -> False) nodes
        findf (Right i)  = find (\z -> case getRoot (current z) of Right (i',_) -> i == i'; _ -> False) leaves
        findf' :: ATNode (Either Range Int) -> Maybe (ATNode (BitreeZipper (Range,ChunkTag) (Int,(POSTag,Text))))
        findf' (SimpleNode e) = SimpleNode <$> findf e
        findf' (LinkedNode e1 e2) = LinkedNode <$> findf e1 <*> findf e2
        replacef :: Maybe (ATNode (Either Range Int))
                 -> Maybe (ATNode (BitreeZipper (Range,ChunkTag) (Int,(POSTag,Text))))
        replacef = join . fmap findf'


mkArgTable :: PennTreeIdx -> [(LinkID,Range)] -> (FilePath,Int,Int) -> [Argument] -> ArgTable (ATNode (Either Range Int))
mkArgTable itr l2p (file,sid,tid) args  =
    ArgTable (T.intercalate " " . map (^._2._2) . toList <$> (findArg (== Relation)))
             (adj <$> findArg (== NumberedArgument 0))
             (adj <$> findArg (== NumberedArgument 1))
             (adj <$> findArg (== NumberedArgument 2))
             (adj <$> findArg (== NumberedArgument 3))
             (adj <$> findArg (== NumberedArgument 4))
             (file,sid,tid)
  where
    adj x@(PL (_i,(D_NONE,t))) = let (_trc,mlid) = identifyTrace t
                                     mlnk = do lid <-mlid
                                               rng <- lookup lid l2p
                                               return rng
                                 in case mlnk of
                                      Nothing -> SimpleNode (bimap fst fst (getRoot x))
                                      Just lnk -> LinkedNode (bimap fst fst (getRoot x)) (Left lnk)
    adj x                      = SimpleNode (bimap fst fst (getRoot x))
    --
    findArg lcond = do a <- find (\a -> lcond (a^.arg_label)) args
                       let ns = a^.arg_terminals
                       case ns of
                         n:_ -> snd <$> findNode n itr
                         _   -> Nothing


mkArgPattern :: Maybe (TP as) -> ArgTable (ATNode a) -> ArgPattern Voice a
mkArgPattern mtp ArgTable {..} =
  ArgPattern { _patt_property = mtp^?_Just.complement.headX.vp_voice
             , _patt_arg0 = fmap chooseATNode _tbl_arg0
             , _patt_arg1 = fmap chooseATNode _tbl_arg1
             , _patt_arg2 = fmap chooseATNode _tbl_arg2
             , _patt_arg3 = fmap chooseATNode _tbl_arg3
             , _patt_arg4 = fmap chooseATNode _tbl_arg4
             }
