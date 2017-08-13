{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module OntoNotes.Type.ArgTable where

import           Control.Lens
import           Data.Hashable
import           Data.Foldable
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text               as T
import           GHC.Generics
--
import           NLP.Syntax.Type
import           NLP.Type.PennTreebankII
import           PropBank.Match
import           PropBank.Type.Prop

data ATNode a = SimpleNode { _atnode_orig :: a }
              | LinkedNode { _atnode_orig  :: a
                           , _atnode_link  :: a } 
              deriving (Show,Functor)



-- now i can experiment flexibly with linked node
chooseATNode (SimpleNode x) = x
-- chooseATNode (LinkedNode x y) = y
chooseATNode (LinkedNode x y) = x
                 
data ArgTable a = ArgTable { _tbl_rel  :: Maybe Text
                           , _tbl_arg0 :: Maybe (ATNode a)
                           , _tbl_arg1 :: Maybe (ATNode a)
                           , _tbl_arg2 :: Maybe (ATNode a)
                           , _tbl_arg3 :: Maybe (ATNode a)
                           , _tbl_arg4 :: Maybe (ATNode a)
                           , _tbl_file_sid_tid :: (FilePath,Int,Int)
                           }
                  deriving (Show)

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
                  deriving (Show,Eq,Ord,Generic)

makeLenses ''ArgPattern


instance Hashable (ArgPattern Text)



mkArgPattern :: Maybe Voice -> ArgTable a -> ArgPattern a
mkArgPattern mvoice ArgTable {..} = ArgPattern { _patt_voice = mvoice
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


phraseNodeType :: PennTreeIdx -> Text
phraseNodeType (PN (_,c) xs) = case c of
                                 PP   -> T.pack (show c) <> maybe "" (\t -> "-" <> t) (headPreposition xs)
                                 ADVP -> case headAdverb xs of
                                           Just t -> T.pack (show c) <> "-" <> t
                                           Nothing -> case headPreposition xs of
                                                        Just t -> "PP-" <> t
                                                        Nothing -> "??ADVP"
                                 PRT  -> "PP" <> maybe "" (\t -> "-" <> t) (headAdverb xs)
                                 WHNP -> "NP"
                                 _     -> T.pack (show c)
phraseNodeType (PL (_,(D_NONE,t))) = t
phraseNodeType (PL (_,(p     ,t))) = case isNoun p of
                                       Yes -> "NP"
                                       _   -> "??" <> T.pack (show (p,t))


mkArgTable :: PennTreeIdx -> [(LinkID,Range)] -> (FilePath,Int,Int) -> [Argument] -> ArgTable Text
mkArgTable itr l2p (file,sid,tid) args  =
    ArgTable (T.intercalate " " . map (^._2._2) . toList <$> (findArg (== Relation)))
             (fmap phraseNodeType . adj <$> findArg (== NumberedArgument 0))
             (fmap phraseNodeType . adj <$> findArg (== NumberedArgument 1))
             (fmap phraseNodeType . adj <$> findArg (== NumberedArgument 2))
             (fmap phraseNodeType . adj <$> findArg (== NumberedArgument 3))
             (fmap phraseNodeType . adj <$> findArg (== NumberedArgument 4))
             (file,sid,tid)
  where
    adj x@(PL (_,(D_NONE,t))) = let (_trc,mlid) = identifyTrace t
                                    mlnk = do lid <-mlid 
                                              rng <- lookup lid l2p
                                              matchR rng itr
                                in case mlnk of
                                     Nothing -> SimpleNode x
                                     Just lnk -> LinkedNode x lnk
    adj x                     = SimpleNode x
    findArg lcond = do a <- find (\a -> lcond (a^.arg_label)) args
                       let ns = a^.arg_terminals
                       case ns of
                         n:_ -> snd <$> findNode n itr 
                         _   -> Nothing

