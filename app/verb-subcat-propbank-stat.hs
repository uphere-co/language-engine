{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

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
import           Data.Hashable
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
import           Data.Attribute
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Syntax.Clause
import           NLP.Syntax.Type
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
import           OntoNotes.App.Load           hiding (Config)
import           OntoNotes.Corpus.Load
import           OntoNotes.Corpus.PropBank
import           OntoNotes.Parser.Sense
import           OntoNotes.Type.ArgTable
import           OntoNotes.Type.Sense
import           OntoNotes.Type.SenseInventory
--
import           Debug.Trace


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




formatArgPatt patt = printf "arg0: %-10s   arg1: %-10s   arg2: %-10s   arg3: %-10s   arg4: %-10s"
                       (fromMaybe "" (patt^.patt_arg0))
                       (fromMaybe "" (patt^.patt_arg1))
                       (fromMaybe "" (patt^.patt_arg2))
                       (fromMaybe "" (patt^.patt_arg3))
                       (fromMaybe "" (patt^.patt_arg4))


formatArgTable mvpmva tbl = printf "%-15s (%-10s)  arg0: %-10s   arg1: %-10s   arg2: %-10s   arg3: %-10s   arg4: %-10s            ## %10s sentence %3d token %3d"
                              (fromMaybe "" (tbl^.tbl_rel))
                              (maybe "unmatched" (\(vp,_) -> show (vp^.vp_voice)) mvpmva)
                              (fromMaybe "" (tbl^.tbl_arg0))
                              (fromMaybe "" (tbl^.tbl_arg1))
                              (fromMaybe "" (tbl^.tbl_arg2))
                              (fromMaybe "" (tbl^.tbl_arg3))
                              (fromMaybe "" (tbl^.tbl_arg4))
                              (tbl^.tbl_file_sid_tid._1)
                              (tbl^.tbl_file_sid_tid._2)
                              (tbl^.tbl_file_sid_tid._3)


formatInst :: Bool  -- ^ show detail?
           -> ((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)
           -> String
formatInst doesShowDetail (filesidtid,corenlp,proptr,inst,sense) =
  let args = inst^.inst_arguments
      lmap = IM.fromList (map (_2 %~ Lemma) (corenlp^._2))
      coretr = corenlp^._1
      
      minst = MatchedInstance { _mi_instance = inst, _mi_arguments = matchArgs (coretr,proptr) inst }
      
      verbprops = verbPropertyFromPennTree lmap coretr 
      clausetr = clauseStructure verbprops (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx coretr))
      l2p = linkID2PhraseNode proptr
      iproptr = mkPennTreeIdx proptr
      argtable = mkArgTable iproptr l2p filesidtid args
      mvpmva = matchVerbPropertyWithRelation verbprops clausetr minst
  in 
     (if doesShowDetail
       then "\n================================================================\n" ++
            T.unpack (formatIndexTokensFromTree 0 proptr)                          ++
            "\n"                                                                   ++
            "\n================================================================\n" ++
            formatMatchedVerb minst mvpmva ++ "\n"
       else ""
     )
     ++ formatArgTable mvpmva argtable
     

formatStatInst doesShowDetail ((sense,sense_num),count) (mdefn,insts) = --- sensedb imap  =
  "\n============================================================================\n"
  ++ printf "%20s : %6d :  %s\n" (sense <> "." <> sense_num) count (fromMaybe "" mdefn)
  ++ "============================================================================\n"
  ++ (intercalate "\n" . map (formatInst doesShowDetail)) insts


printHeader (lma,f) = do
  let headstr = printf "%20s:%6d" lma f :: String
  putStrLn "\n\n\n\n\n*************************************************************"
  putStrLn "*************************************************************"
  putStrLn "****                                                     ****"
  putStrLn (printf "****             %27s             ****" headstr)
  putStrLn "****                                                     ****"    
  putStrLn "*************************************************************"
  putStrLn "*************************************************************"


getDefInst :: HashMap Text Inventory
           -> HashMap (Text,Text) [((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)]
           -> (Text,Text)
           -> (Maybe Text, [((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)])
getDefInst sensedb imap (sense,sense_num) =
  let mdefn = do inv <- HM.lookup sense sensedb
                 (^.sense_name) <$> find (\s->s^.sense_n == sense_num) (inv^.inventory_senses)
      insts = fromMaybe [] (HM.lookup (sense,sense_num) imap)
  in (mdefn,insts)


filterSenseByLemma lma = sortBy (compare `on` (^._1))
                       . map (_2 %~ length)
                       . filter (\((sense,_),_) -> sense == lma <> "-v") 


showStat :: HashMap Text Inventory
         -> [(Text,Int)]                -- ^ lemmastat
         -> HashMap (Text,Text) Int     -- ^ sensestat
         -> HashMap (Text,Text) [((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)]
         -> IO ()
showStat sensedb lemmastat sensestat classified_inst_map = do
  let lst = HM.toList classified_inst_map
  forM_ lemmastat $ \(lma,f) -> do
    printHeader (lma,f)
    -- let lst' = filterSenseByLemma lma lst
    forM_ (filterSenseByLemma lma lst) $ \((sense,sense_num),count) -> do
      let (mdefn,insts) = getDefInst sensedb classified_inst_map (sense,sense_num)
          statmap = foldl' addfunc HM.empty insts
            where addfunc acc (filesidtid,_,proptr,inst,sense) =
                    let args = inst^.inst_arguments
                        iproptr = mkPennTreeIdx proptr
                        l2p = linkID2PhraseNode proptr
                        argtable = mkArgTable iproptr l2p filesidtid args
                        argpatt = mkArgPattern argtable
                    in HM.alter (\case Nothing -> Just 1 ; Just n -> Just (n+1)) argpatt acc
          statlst = (sortBy (flip compare `on` snd) . HM.toList) statmap
          senseheader = "\n============================================================================\n"
                        ++ printf "%20s : %6d :  %s\n" (sense <> "." <> sense_num) count (fromMaybe "" mdefn)
                        ++ "============================================================================\n"
      putStrLn senseheader
      forM_ statlst $ \(patt :: ArgPattern,n :: Int) -> do
        let str1 = formatArgPatt patt :: String
        putStrLn (printf "%s     #count: %5d" str1 n)


showStatInst :: Bool
             -> HashMap Text Inventory
             -> [(Text,Int)]                -- ^ lemmastat
             -> HashMap (Text,Text) Int     -- ^ sensestat
             -> HashMap (Text,Text) [((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)]
             -> IO ()
showStatInst doesShowDetail sensedb lemmastat sensestat classified_inst_map = do
  let lst = HM.toList classified_inst_map
  forM_ lemmastat $ \(lma,f) -> do
    printHeader (lma,f)
    forM_ (filterSenseByLemma lma lst) $ \x -> do
      let definsts = getDefInst sensedb classified_inst_map (fst x)
          str = formatStatInst doesShowDetail x definsts
      putStrLn str




showError (Left err) = print err
showError (Right _) = return ()


readSenseInsts sensefile = fmap (rights . map parseSenseInst . map T.words . T.lines) (T.IO.readFile sensefile)




data ProgOption = ProgOption { showDetail :: Bool
                             , statOnly :: Bool
                             } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> switch (long "detail" <> short 'd' <> help "Whether to show detail")
                      <*> switch (long "stat" <> short 's' <> help "Calculate statistics")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "PropBank statistics relevant to verb subcategorization")




-- need parse tree to adjust index (deleting none and hyphen)
mergePropSense proptr insts senses =
  let nonelist = map fst . filter (isNone.fst.snd) . zip [0..] . toList . getADTPennTree 
      adj = adjustIndex (nonelist proptr)
      lst = map (\x -> fromTuple (x^.sinst_token_id,x)) senses
  in map toTuple (joinAttrib (\x -> (either id id . adj) (x^.inst_predicate_id)) insts lst)
      

main = do
  opt <- execParser progOption
  
  propdb <- constructFrameDB (cfg^.cfg_propbank_framedir)
  let preddb = constructPredicateDB propdb

  sensedb <- HM.fromList . map (\si->(si^.inventory_lemma,si)) <$> loadSenseInventory (cfg^.cfg_sense_inventory_file)  
  
  dtr <- build (cfg^.cfg_wsj_directory)
  let fps = {- Prelude.take 200 $ -} sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".parse") fps
      propfiles  = filter (\x -> takeExtensions x == ".prop" ) fps      
      sensefiles = filter (\x -> takeExtensions x == ".sense") fps
      lst = map (\x -> fromTuple (takeBaseName x,x)) parsefiles
      joiner = joinAttrib takeBaseName 
     
      lst' = mapMaybe (\(i,mf1,mf2,f3) -> (i,,,) <$> mf1 <*> mf2 <*> pure f3)
           . map toTuple $ sensefiles `joiner` (propfiles `joiner` lst)
      
  matchedpairs <- fmap (concat . catMaybes) $ do
    flip traverse lst' $ \(article,sensefile,propfile,parsefile) -> do
      hPutStrLn stderr article
      (mprops :: Maybe [(Int,(_,_))])
        <- join . eitherToMaybe <$>
             runEitherT (loadMatchArticle (cfg^.cfg_wsj_corenlp_directory) (cfg^.cfg_wsj_directory) article)
      senses <- readSenseInsts sensefile
      let match (i,r) = let ss = filter (\s -> s^.sinst_sentence_id == i) senses
                        in ((article,i),(r,ss))
      return . fmap (map match) $ mprops
      
  let flatMatchedPairs = do ((f,sid),((((coretr,_,corelma),proptr),insts),senses)) <- matchedpairs
                            (tid,minst,sense) <- mergePropSense proptr insts senses
                            inst <- maybeToList minst
                            return ((f,sid,tid),(coretr,corelma),proptr,inst,sense)
      insts_v = filter (\p->T.last (p^._4.inst_lemma_type) == 'v') flatMatchedPairs
      classified_inst_map = foldl' addfunc  HM.empty insts_v
          where addfunc acc x = HM.insertWith (++) (x^._5.to getSenseID) [x] acc

  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  rolesetstat <- loadStatistics (cfg^.cfg_statistics)
  let lemmastat = mergeStatPB2Lemma rolesetstat

  if (statOnly opt)
    then showStat sensedb lemmastat sensestat classified_inst_map 
    else showStatInst (showDetail opt) sensedb lemmastat sensestat classified_inst_map



