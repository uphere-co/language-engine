{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import qualified Data.Attoparsec.Text       as A
import           Data.Either.Extra
import           Data.Foldable
import           Data.Function                      (on)
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text                          (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Data.Traversable
import           Options.Applicative
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.Printf
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Syntax.Clause
import           NLP.Syntax.Verb
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           PropBank.Format
import           PropBank.Parser.Prop
import           PropBank.Match
import           PropBank.Query
import           PropBank.Type.Frame          hiding (ProgOption)
import           PropBank.Type.Match
import           PropBank.Type.Prop
import           PropBank.Util                       (merge)
--
import           OntoNotes.Corpus.Load
import           OntoNotes.Corpus.PropBank


type LemmaList = [(Int,Text)]

lookupRoleset :: PredicateDB -> (Text,Text) -> Maybe Text
lookupRoleset db (lma,sens) = do
  p <- HM.lookup lma (db^.predicateDB)
  let rs = p ^.predicate_roleset
  defn <- Data.List.lookup (lma <> "." <> sens) $ map (\r -> (r^.roleset_id,fromMaybe "" (r^.roleset_name))) rs
  return defn

maybeNumberedArgument :: PropBankLabel -> Maybe Int
maybeNumberedArgument (NumberedArgument n) = Just n
maybeNumberedArgument _                    = Nothing

data ArgTable = ArgTable { _tbl_rel  :: Maybe Text
                         , _tbl_arg0 :: Maybe Text
                         , _tbl_arg1 :: Maybe Text
                         , _tbl_arg2 :: Maybe Text
                         , _tbl_arg3 :: Maybe Text
                         , _tbl_arg4 :: Maybe Text } 

makeLenses ''ArgTable

mkArgTable :: PennTree -> [Argument] -> ArgTable
mkArgTable tr args = ArgTable (findArg Relation)
                              (findArg (NumberedArgument 0))
                              (findArg (NumberedArgument 1))
                              (findArg (NumberedArgument 2))
                              (findArg (NumberedArgument 3))
                              (findArg (NumberedArgument 4))
  where
    itr = mkPennTreeIdx tr
    findArg l = do a <- find (\a -> a^.arg_label == l) args
                   let ns = a^.arg_terminals
                   return (formatArgNodes tr ns)


formatArgTable tbl = printf "%-10s   arg0:%-10s   arg1:%-10s    arg2:%-10s   arg3:%-10s   arg4:%-10s"
                       (fromMaybe "" (tbl^.tbl_rel))
                       (fromMaybe "" (tbl^.tbl_arg0))
                       (fromMaybe "" (tbl^.tbl_arg1))
                       (fromMaybe "" (tbl^.tbl_arg2))
                       (fromMaybe "" (tbl^.tbl_arg3))
                       (fromMaybe "" (tbl^.tbl_arg4))



formatArgNodes :: PennTree -> [Node] -> Text
formatArgNodes tr nodes = T.intercalate "\n" . map (T.pack . show . snd) . mapMaybe (\n -> findNode n tr) $  nodes
  -- let nodes = arg^.arg_terminals


formatInst :: Bool  -- ^ show detail?
           -> (Int,(PennTree,LemmaList),PennTree,Instance)
           -> String
formatInst doesShowDetail (_,corenlp,proptr,inst) =
  let args = inst^.inst_arguments
      lmap = IM.fromList (map (_2 %~ Lemma) (corenlp^._2))
      coretr = corenlp^._1
      
      minst = MatchedInstance { _mi_instance = inst, _mi_arguments = matchArgs (coretr,proptr) inst }
      
      verbprops = verbPropertyFromPennTree lmap coretr 
      clausetr = clauseStructure verbprops (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx coretr))
      -- iproptr = mkPennTreeIdx proptr
      argtable = mkArgTable proptr args
      
  in "\n================================================================\n" ++
     T.unpack (formatIndexTokensFromTree 0 proptr)                          ++
     "\n"                                                                   ++
     "\n================================================================\n" ++
     if doesShowDetail then formatPropMatch verbprops clausetr minst ++ "\n" else "" ++
     formatArgTable argtable
                                                                                  
     -- (intercalate "\n--------------------------------------------------------------\n" $
        {- flip map args $ \arg ->
          show (arg^.arg_label) ++ "\n" ++ (T.unpack (formatArgNodes proptr arg)) -}
     -- )
   --  T.unpack (prettyPrint 0 tr) ++ "\n" ++ show args



formatStatInst :: Bool           -- ^ show detail?
               -> PredicateDB
               -> HashMap RoleSetID [(Int,(PennTree,LemmaList),PennTree,Instance)]
               -> (RoleSetID,Int)
               -> String
formatStatInst doesShowDetail db imap (rid,num) =
  let mdefn = lookupRoleset db rid
      minsts = HM.lookup rid imap
  in printf "%20s : %5d : %s\n" (formatRoleSetID rid) num  (fromMaybe "" mdefn)
     ++ (intercalate "\n" . map (formatInst doesShowDetail) . concat . maybeToList) minsts

 
showStatInst doesShowDetail preddb classified_inst_map = do
  let lst = HM.toList classified_inst_map
      stat = sortBy (flip compare `on` (^._2)) $ map (_2 %~ length) lst
  mapM_ (putStrLn . formatStatInst doesShowDetail preddb classified_inst_map) stat



showError (Left err) = print err
showError (Right _) = return ()









data ProgOption = ProgOption { showDetail :: Bool
                             } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> switch (long "detail" <> short 'd' <> help "Whether to show detail")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "PropBank statistics relevant to verb subcategorization")



main = do
  opt <- execParser progOption
  
  let propframedir = "/home/wavewave/repo/srcc/propbank-frames/frames" 
  propdb <- constructFrameDB propframedir
  let preddb = constructPredicateDB propdb

  let corenlpdir = "/scratch/wavewave/run/ontonotes_corenlp_ptree_udep_lemma_20170710"
      basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"

  dtr <- build basedir
  let fps = Prelude.take 100 $ sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".prop") fps
      
  parsedpairs <- fmap (concat . catMaybes) $ do
    flip traverse parsefiles $ \f -> do
      let article  = takeBaseName f
      -- print article
      join . eitherToMaybe <$> runEitherT (loadMatchArticle corenlpdir basedir article)
      
  let flatParsedPairs = do (i,(((coretr,_,corelma),proptr),insts)) <- parsedpairs
                           inst <- insts
                           return (i,(coretr,corelma),proptr,inst)
  let insts_v = filter (\p->T.last (p^._4.inst_lemma_type) == 'v') flatParsedPairs
      classified_inst_map = foldl' addfunc  HM.empty insts_v
          where addfunc acc x = HM.insertWith (++) (x^._4.inst_lemma_roleset_id) [x] acc
  showStatInst (showDetail opt) preddb classified_inst_map
 
