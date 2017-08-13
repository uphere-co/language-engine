{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Either
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
import           Options.Applicative         hiding (str)
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.Printf
--
import           Data.Attribute
import           NLP.Printer.PennTreebankII
import           NLP.Syntax.Clause
import           NLP.Syntax.Type
import           NLP.Syntax.Verb
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           PropBank.Match
import           PropBank.Query
import           PropBank.Type.Frame          hiding (ProgOption)
import           PropBank.Type.Match
import           PropBank.Type.Prop
--
import           OntoNotes.App.Load           hiding (Config)
import           OntoNotes.Corpus.Load
import           OntoNotes.Corpus.PropBank
import           OntoNotes.Format
import           OntoNotes.Parser.Sense
import           OntoNotes.Type.ArgTable
import           OntoNotes.Type.Sense
import           OntoNotes.Type.SenseInventory
--


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


formatArgMap isStat argmap = 
  (if isStat
     then printf " %-20s " (fromMaybe "frame" (lookup "frame" argmap))
     else printf " %-20s         " (fromMaybe "frame" (lookup "frame" argmap)))
  ++ printf "arg0: %-10s   arg1: %-10s   arg2: %-10s   arg3: %-10s   arg4: %-10s\n"
       (fromMaybe "" (lookup "arg0" argmap))
       (fromMaybe "" (lookup "arg1" argmap))
       (fromMaybe "" (lookup "arg2" argmap))
       (fromMaybe "" (lookup "arg3" argmap))
       (fromMaybe "" (lookup "arg4" argmap))
  ++ "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"


formatArgTable :: Maybe (VerbProperty (BitreeZipperICP '[Lemma]),_) -> ArgTable Text -> String
formatArgTable mvpmva tbl = printf "%-15s (%-10s)  arg0: %-10s   arg1: %-10s   arg2: %-10s   arg3: %-10s   arg4: %-10s            ## %10s sentence %3d token %3d"
                              (fromMaybe "" (tbl^.tbl_rel))
                              (maybe "unmatched" (\(vp,_) -> show (vp^.vp_voice)) mvpmva)
                              (fromMaybe "" (tbl^.tbl_arg0.to (fmap chooseATNode)))
                              (fromMaybe "" (tbl^.tbl_arg1.to (fmap chooseATNode)))
                              (fromMaybe "" (tbl^.tbl_arg2.to (fmap chooseATNode)))
                              (fromMaybe "" (tbl^.tbl_arg3.to (fmap chooseATNode)))
                              (fromMaybe "" (tbl^.tbl_arg4.to (fmap chooseATNode)))
                              (tbl^.tbl_file_sid_tid._1)
                              (tbl^.tbl_file_sid_tid._2)
                              (tbl^.tbl_file_sid_tid._3)


formatInst :: Bool  -- ^ show detail?
           -> Maybe [(Text,Text)] 
           -> ((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)
           -> String
formatInst doesShowDetail margmap (filesidtid,corenlp,proptr,inst,_sense) =
  let args = inst^.inst_arguments
      lemmamap = IM.fromList (map (_2 %~ Lemma) (corenlp^._2))
      coretr = corenlp^._1
      
      minst = MatchedInstance { _mi_instance = inst, _mi_arguments = matchArgs (coretr,proptr) inst }
      
      verbprops = verbPropertyFromPennTree lemmamap coretr 
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


getArgMapFromRoleMap (lma,sense_num) rolemap = (^._2) <$> find f rolemap
  where f rm = if lma == "hold" && sense_num == "5"       -- this is an ad hoc treatment for group 2
               then rm^._1 == (lma,"2." <> sense_num) 
               else rm^._1 == (lma,"1." <> sense_num)


formatStatInst :: Bool
               -> [((Text,Text),[(Text,Text)])]
               -> Text
               -> ((Text,Text),Int)
               -> (Maybe Text, [((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)])
               -> String
formatStatInst doesShowDetail rolemap lma ((sense,sense_num),count) (mdefn,insts) =
  let sensetxt = (sense <> "." <> sense_num)
      margmap = getArgMapFromRoleMap (lma,sense_num) rolemap
  in "\n============================================================================\n"
     ++ printf "%20s : %6d :  %s\n" sensetxt  count (fromMaybe "" mdefn)
     ++ "============================================================================\n"
     ++ maybe "" (formatArgMap False) margmap 
     ++ (intercalate "\n" . map (formatInst doesShowDetail margmap)) insts


printHeader :: (Text,Int) -> IO ()
printHeader (lma,count) = do
  let headstr = printf "%20s:%6d" lma count :: String
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
getDefInst sensedb instmap (sense,sense_num) =
  let mdefn = do inv <- HM.lookup sense sensedb
                 (^.sense_name) <$> find (\s->s^.sense_n == sense_num) (inv^.inventory_senses)
      insts = fromMaybe [] (HM.lookup (sense,sense_num) instmap)
  in (mdefn,insts)


countSenseForLemma :: Text -> [((Text,Text),[a])] -> [((Text,Text),Int)]
countSenseForLemma lma = sortBy (compare `on` (^._1))
                           . map (_2 %~ length)
                           . filter (\((sense,_),_) -> sense == lma <> "-v") 


showStat :: Bool                        -- ^ is tab separated format
         -> [((Text,Text),[(Text,Text)])] 
         -> HashMap Text Inventory
         -> [(Text,Int)]                -- ^ lemmastat
         -> HashMap (Text,Text) [((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)]
         -> IO ()
showStat isTSV rolemap sensedb lemmastat classified_inst_map = do
  let lst = HM.toList classified_inst_map
  forM_ lemmastat $ \(lma,f) -> do
    when (not isTSV) $ printHeader (lma,f)
    forM_ (countSenseForLemma lma lst) $ \((sense,sense_num),count) -> do
      let (mdefn,insts) = getDefInst sensedb classified_inst_map (sense,sense_num)
          statmap = foldl' addfunc HM.empty insts
            where addfunc acc (filesidtid,corenlp,proptr,inst,_sense) =
                    let args = inst^.inst_arguments
                        iproptr = mkPennTreeIdx proptr
                        l2p = linkID2PhraseNode proptr
                        coretr = corenlp^._1
                        minst = MatchedInstance { _mi_instance = inst, _mi_arguments = matchArgs (coretr,proptr) inst }
                        
                        lemmamap = IM.fromList (map (_2 %~ Lemma) (corenlp^._2))
                        verbprops = verbPropertyFromPennTree lemmamap coretr 
                        
                        clausetr = clauseStructure verbprops (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx coretr))
                        mvpmva = matchVerbPropertyWithRelation verbprops clausetr minst
                        mvoice = do (vp,_) <- mvpmva
                                    return (vp^.vp_voice)
                        argtable = mkArgTable iproptr l2p filesidtid args
                        argpatt = mkArgPattern mvoice argtable
                    in HM.alter (\case Nothing -> Just 1 ; Just n -> Just (n+1)) argpatt acc
          statlst = (sortBy (flip compare `on` snd) . HM.toList) statmap
          sensetxt = sense <> "." <> sense_num          
          senseheader = "\n============================================================================\n"
                        ++ printf "%20s : %6d :  %s\n" sensetxt count (fromMaybe "" mdefn)
                        ++ "============================================================================\n"
      if (not isTSV)
        then do
          putStrLn senseheader
          let margmap = getArgMapFromRoleMap (lma,sense_num) rolemap 
          traverse_ (putStrLn . formatArgMap True) margmap
          forM_ statlst $ \(patt,n :: Int) -> do
            let str1 = formatArgPatt patt :: String
            putStrLn (printf "%s     #count: %5d" str1 n)
        else do
          forM_ statlst $ \(patt, n :: Int) -> do
            putStrLn $ printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%d"
                         sense
                         sense_num
                         (maybe "null" show (patt^.patt_voice)) 
                         (fromMaybe "null" (patt^.patt_arg0))
                         (fromMaybe "null" (patt^.patt_arg1))
                         (fromMaybe "null" (patt^.patt_arg2))
                         (fromMaybe "null" (patt^.patt_arg3))
                         (fromMaybe "null" (patt^.patt_arg4))
                         n

showStatInst :: Bool
             -> [((Text,Text),[(Text,Text)])] 
             -> HashMap Text Inventory
             -> [(Text,Int)]                -- ^ lemmastat
             -> HashMap (Text,Text) [((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)]
             -> IO ()
showStatInst doesShowDetail rolemap sensedb lemmastat classified_inst_map = do
  let lst = HM.toList classified_inst_map
  forM_ lemmastat $ \(lma,freq) -> do
    when (freq /= 0) $ do
      printHeader (lma,freq)
      forM_ (countSenseForLemma lma lst) $ \x -> do
        let definsts = getDefInst sensedb classified_inst_map (fst x)
            str = formatStatInst doesShowDetail rolemap lma x definsts
        putStrLn str


showError :: Either String a -> IO ()
showError (Left e)  = print e
showError (Right _) = return ()


readSenseInsts :: FilePath -> IO [SenseInstance]
readSenseInsts sensefile = fmap (rights . map parseSenseInst . map T.words . T.lines) (T.IO.readFile sensefile)

-- need parse tree to adjust index (deleting none and hyphen)
mergePropSense :: PennTree -> [Instance] -> [SenseInstance] -> [(Int, Maybe Instance, SenseInstance)]
mergePropSense proptr insts senses =
  let nonelist = map fst . filter (isNone.fst.snd) . zip [0..] . toList . getADTPennTree 
      adj = adjustIndex (nonelist proptr)
      lst = makeKeyAttrib (^.sinst_token_id) senses --  map (\x -> fromTuple (x^.sinst_token_id,x)) senses
  in map toTuple (joinAttrib (\x -> (either id id . adj) (x^.inst_predicate_id)) insts lst)
      



data ProgOption = ProgOption { showDetail :: Bool
                             , statOnly   :: Bool
                             , tsvFormat  :: Bool
                             } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> switch (long "detail" <> short 'd' <> help "Whether to show detail")
                      <*> switch (long "stat" <> short 's' <> help "Calculate statistics")
                      <*> switch (long "tsv" <> short 't' <> help "tsv format")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "PropBank statistics relevant to verb subcategorization")


main :: IO ()
main = do
  opt <- execParser progOption
  sensedb <- HM.fromList . map (\si->(si^.inventory_lemma,si)) <$> loadSenseInventory (cfg^.cfg_sense_inventory_file)  
  
  dtr <- build (cfg^.cfg_wsj_directory)
  let fps = Prelude.take 200 $ sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".parse") fps
      propfiles  = filter (\x -> takeExtensions x == ".prop" ) fps      
      sensefiles = filter (\x -> takeExtensions x == ".sense") fps
      lst = map (\x -> fromTuple (takeBaseName x,x)) parsefiles
      joiner = joinAttrib takeBaseName 
     
      lst' = mapMaybe (\(i,mf1,mf2,f3) -> (i,,,) <$> mf1 <*> mf2 <*> pure f3)
           . map toTuple $ sensefiles `joiner` (propfiles `joiner` lst)
      
  matchedpairs <- fmap (concat . catMaybes) $ do
    flip traverse lst' $ \(article,sensefile,_propfile,_parsefile) -> do
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

  -- sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  rolesetstat <- loadStatistics (cfg^.cfg_statistics)
  let lemmastat = mergeStatPB2Lemma rolesetstat
  let rolemapfile = "/home/wavewave/repo/srcp/OntoNotes/mapping/final.txt"
  rolemap <- loadRoleMap rolemapfile
  
  if (statOnly opt)
    then showStat (tsvFormat opt) rolemap sensedb lemmastat classified_inst_map 
    else showStatInst (showDetail opt) rolemap sensedb lemmastat classified_inst_map



