{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module OntoNotes.Type.ArgTable where

import           Control.Lens
import           Data.Foldable
import           Data.List
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text               as T
--
import           NLP.Type.PennTreebankII
import           PropBank.Match
import           PropBank.Type.Frame
import           PropBank.Type.Prop
  
data ArgTable = ArgTable { _tbl_rel  :: Maybe Text
                         , _tbl_arg0 :: Maybe Text
                         , _tbl_arg1 :: Maybe Text
                         , _tbl_arg2 :: Maybe Text
                         , _tbl_arg3 :: Maybe Text
                         , _tbl_arg4 :: Maybe Text
                         , _tbl_file_sid_tid :: (FilePath,Int,Int)
                         } 

makeLenses ''ArgTable

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

 
phraseNodeType (PN (_,c) xs) = case c of
                                 PP   -> T.pack (show c) <> maybe "" (\t -> "-" <> t) (headPreposition xs)
                                 ADVP -> case headAdverb xs of
                                           Just t -> T.pack (show c) <> "-" <> t
                                           Nothing -> case headPreposition xs of
                                                        Just t -> T.pack (show c) <> "-" <> "PP" <> "-" <> t
                                                        Nothing -> T.pack (show c)
                                 WHNP -> "NP"
                                 _     -> T.pack (show c)
phraseNodeType (PL (_,(D_NONE,t))) = t
phraseNodeType (PL (_,(p     ,t))) = case isNoun p of
                                       Yes -> "NP"
                                       _   -> "??" <> T.pack (show (p,t))


mkArgTable :: PennTreeIdx -> [(LinkID,Range)] -> (FilePath,Int,Int) -> [Argument] -> ArgTable
mkArgTable itr l2p (file,sid,tid) args  =
    ArgTable (T.intercalate " " . map (^._2._2) . toList <$> (findArg (== Relation)))
             (phraseNodeType . adj <$> findArg (== NumberedArgument 0))
             (phraseNodeType . adj <$> findArg (== NumberedArgument 1))
             (phraseNodeType . adj <$> findArg (== NumberedArgument 2))
             (phraseNodeType . adj <$> findArg (== NumberedArgument 3))
             (phraseNodeType . adj <$> findArg (== NumberedArgument 4))
             (file,sid,tid)
  where
    adj x@(PL (_,(D_NONE,t))) = let (trc,mlid) = identifyTrace t
                                    mlnk = do lid <-mlid 
                                              rng <- lookup lid l2p
                                              matchR rng itr
                                in case mlnk of
                                     Nothing -> x
                                     Just lnk -> lnk
    adj x                     = x                     
    findArg lcond = do a <- find (\a -> lcond (a^.arg_label)) args
                       let ns = a^.arg_terminals
                       case ns of
                         n:_ -> snd <$> findNode n itr 
                         _   -> Nothing

