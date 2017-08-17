{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module OntoNotes.Type.ArgTable where

import           Control.Lens
import           Control.Monad                (join)
import           Data.Hashable
import           Data.Foldable
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text               as T
import           Data.Traversable
import           GHC.Generics
--
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Syntax.Type
import           NLP.Type.PennTreebankII
import           PropBank.Match
import           PropBank.Type.Prop

data ATNode a = SimpleNode { _atnode_orig :: a }
              | LinkedNode { _atnode_orig  :: a
                           , _atnode_link  :: a } 
              deriving (Show,Functor,Foldable,Traversable)



-- now i can experiment flexibly with linked node
chooseATNode (SimpleNode x) = x
-- chooseATNode (LinkedNode x y) = y
chooseATNode (LinkedNode x y) = x
                 
data ArgTable b = ArgTable { _tbl_rel  :: Maybe Text
                           , _tbl_arg0 :: Maybe b
                           , _tbl_arg1 :: Maybe b
                           , _tbl_arg2 :: Maybe b
                           , _tbl_arg3 :: Maybe b
                           , _tbl_arg4 :: Maybe b
                           , _tbl_file_sid_tid :: (FilePath,Int,Int)
                           }
                  deriving (Show,Functor,Foldable,Traversable)

makeLenses ''ArgTable

-- orphan instance! I will move it to syntactic-analysis soon
deriving instance Generic Voice

instance Hashable Voice


data ArgPattern a = ArgPattern { _patt_voice :: Maybe Voice
                               , _patt_arg0 :: Maybe a
                               , _patt_arg1 :: Maybe a
                               , _patt_arg2 :: Maybe a
                               , _patt_arg3 :: Maybe a
                               , _patt_arg4 :: Maybe a
                               }
                  deriving (Show,Eq,Ord,Generic,Functor,Foldable,Traversable)

makeLenses ''ArgPattern


instance Hashable (ArgPattern Text)



mkArgPattern :: Maybe TP -> ArgTable (ATNode a) -> ArgPattern a
mkArgPattern mtp ArgTable {..} = ArgPattern { _patt_voice = mtp^?_Just.tp_VP.vp_verbProperty.vp_voice
                                            , _patt_arg0 = fmap chooseATNode _tbl_arg0
                                            , _patt_arg1 = fmap chooseATNode _tbl_arg1
                                            , _patt_arg2 = fmap chooseATNode _tbl_arg2
                                            , _patt_arg3 = fmap chooseATNode _tbl_arg3
                                            , _patt_arg4 = fmap chooseATNode _tbl_arg4
                                            }




-- | https://en.wiktionary.org/wiki/Category:English_prepositions
-- listOfPrepositions = [ ""
--                      ] 


headPreposition :: [PennTreeIdx] -> Maybe Text
headPreposition xs = getFirst (foldMap (First . f) xs)   
  where f (PN _ _)        = Nothing
        f (PL (_,(IN,t))) = Just (T.toLower t)
        f (PL (_,(TO,t))) = Just (T.toLower t)
        f (PL _         ) = Nothing        


headAdverb :: [PennTreeIdx] -> Maybe Text
headAdverb xs = getLast (foldMap (Last . f) xs)   
  where f (PN _ _)         = Nothing
        f (PL (_,(pos,t))) = if isAdverb pos then Just (T.toLower t) else Nothing


phraseNodeType :: Maybe TP -> BitreeZipper (Range,ChunkTag) (Int,(POSTag,Text)) -> Text
phraseNodeType mtp z
  = let rng = getRange (current z)
        subj = maybe "" (\b -> if b then "-SBJ" else "") $ do tp <- mtp
                                                              dp <- tp^.tp_DP
                                                              return (getRange (current dp) == rng)
        phrase = case current z of
                   PN (_,c) xs       -> case c of
                                          PP   -> T.pack (show c) <> maybe "" (\t -> "-" <> t) (headPreposition xs)
                                          ADVP -> case headAdverb xs of
                                                    Just t -> T.pack (show c) <> "-" <> t
                                                    Nothing -> case headPreposition xs of
                                                                 Just t -> "PP-" <> t
                                                                 Nothing -> "??ADVP"
                                          PRT  -> "PP" <> maybe "" (\t -> "-" <> t) (headAdverb xs)
                                          WHNP -> "NP" <> subj
                                          _     -> T.pack (show c) <> subj
                   PL (_,(D_NONE,t)) -> case parent z of
                                          Nothing -> "??"<> t
                                          Just z' -> phraseNodeType mtp z'
                   PL (_,(p     ,t)) -> case isNoun p of
                                          Yes -> "NP" <> subj
                                          _   -> "??" <> T.pack (show (p,t))
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
    adj x@(PL (i,(D_NONE,t))) = let (_trc,mlid) = identifyTrace t
                                    mlnk = do lid <-mlid 
                                              rng <- lookup lid l2p
                                              return rng
                                              -- matchR rng itr
                                in case mlnk of
                                     Nothing -> SimpleNode (bimap fst fst (getRoot x))
                                     Just lnk -> LinkedNode (bimap fst fst (getRoot x)) (Left lnk)
    adj x                     = SimpleNode (bimap fst fst (getRoot x))
    findArg lcond = do a <- find (\a -> lcond (a^.arg_label)) args
                       let ns = a^.arg_terminals
                       case ns of
                         n:_ -> snd <$> findNode n itr 
                         _   -> Nothing
