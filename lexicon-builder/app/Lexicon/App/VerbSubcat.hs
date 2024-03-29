{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Lexicon.App.VerbSubcat where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Either.Extra (eitherToMaybe,rights)
import           Data.Foldable
import           Data.Function                          (on)
import           Data.HashMap.Strict                    (HashMap)
import qualified Data.HashMap.Strict              as HM
import           Data.IntMap                            (IntMap)
import qualified Data.IntMap                      as IM
import           Data.List
import           Data.Maybe
import           Data.Monoid                            ((<>))
import           Data.Text                              (Text)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TIO
import           Formatting                             ((%),(%.),stext,int,sformat,left,right)
import           System.FilePath
import           System.IO
--
import           Data.Attribute
import           Data.Bitree                            (getRoot)
import           Data.BitreeZipper
import           Lexicon.Format                         (formatArgPatt, formatGRel)
import           Lexicon.Type
import           Lexicon.Query
import           NLP.Printer.PennTreebankII
import           NLP.Syntax.Argument
import           NLP.Syntax.Clause
import           NLP.Syntax.Clause.Old
import           NLP.Syntax.Format
import           NLP.Syntax.Type                        (PredArgWorkspace,STag)
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                        (mkPreAnalysis)
import           NLP.Syntax.Verb
import           NLP.Type.CoreNLP                       (Dependency)
import           NLP.Type.PennTreebankII
import           NLP.Type.PennTreebankII.Match
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.SyntaxProperty                (Voice)
import           OntoNotes.Corpus.Load
import           OntoNotes.Parser.Sense
import           OntoNotes.Type.Sense
import           OntoNotes.Type.SenseInventory
import           PropBank.Match
import           PropBank.Query
import           PropBank.Type.Frame             hiding (ProgOption,Voice)
import           PropBank.Type.Match
import           PropBank.Type.Prop
--
import           Lexicon.Data


type LemmaList = [(Int,Text)]


matchVerbPropertyWithRelation :: PreAnalysis '[Lemma]
                              -> [VerbProperty (BitreeZipperICP '[Lemma])]
                              -> Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text)))
                              -> MatchedInstance
                              -> Maybe (VerbProperty (BitreeZipperICP '[Lemma])
                                       ,Maybe (PredArgWorkspace (Either (Range,STag) (Int,POSTag))))
matchVerbPropertyWithRelation tagged verbprops clausetr minst = do
  relidx <- findRelNode (minst^.mi_arguments)
  vp <- find (\vp->vp^.vp_index==relidx) verbprops
  let x'trs = identifyCPHierarchy tagged verbprops
      mpa = findPAWS tagged clausetr vp x'trs
  return (vp,mpa)




lookupRoleset :: PredicateDB -> (Text,Text) -> Maybe Text
lookupRoleset db (lma,sens) = do
  p <- HM.lookup lma (db^.predicateDB)
  let rs = p ^.predicate_roleset
  defn <- Data.List.lookup (lma <> "." <> sens) $ map (\r -> (r^.roleset_id,fromMaybe "" (r^.roleset_name))) rs
  return defn


maybeNumberedArgument :: PropBankLabel -> Maybe Int
maybeNumberedArgument (NumberedArgument n) = Just n
maybeNumberedArgument _                    = Nothing


formatArgMap :: Bool -> [(Text,Text)] -> Text
formatArgMap isStat argmap =
  (if isStat
     then sformat (" " % ls 20 % " ") (fromMaybe "frame" (lookup "frame" argmap))
     else sformat (" " % ls 20 % "         ") (fromMaybe "frame" (lookup "frame" argmap)))
  <> sformat ("arg0: " % ls 10 % "   arg1: " % ls 10 % "   arg2: " % ls 10 % "   arg3: " % ls 10 % "   arg4: " % ls 10 % "\n")
       (fromMaybe "" (lookup "arg0" argmap))
       (fromMaybe "" (lookup "arg1" argmap))
       (fromMaybe "" (lookup "arg2" argmap))
       (fromMaybe "" (lookup "arg3" argmap))
       (fromMaybe "" (lookup "arg4" argmap))
  <> "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"
  where
    ls n = left n ' ' %. stext

formatArgTable :: Maybe (VerbProperty (Zipper '[Lemma]), Maybe (PredArgWorkspace (Either (Range,STag) (Int,POSTag))))
               -> ArgTable GRel
               -> Text
formatArgTable mvpmva tbl =
    sformat (ls 15 % " (" % ls 10
                   % ")  arg0: " % ls 10
                   % "   arg1: " % ls 10
                   % "   arg2: " % ls 10
                   % "   arg3: " % ls 10
                   % "   arg4: " % ls 10
                   % "            ## " % ls 10
                   % " sentence " % ri 3 % " token " % ri 3)
            (fromMaybe "" (tbl^.tbl_rel))
            (maybe "unmatched" (\(vp,_) -> T.pack (show (vp^.vp_voice))) mvpmva)
            (maybe "" formatGRel (tbl^.tbl_arg0))
            (maybe "" formatGRel (tbl^.tbl_arg1))
            (maybe "" formatGRel (tbl^.tbl_arg2))
            (maybe "" formatGRel (tbl^.tbl_arg3))
            (maybe "" formatGRel (tbl^.tbl_arg4))
            (T.pack (tbl^.tbl_file_sid_tid._1))
            (tbl^.tbl_file_sid_tid._2)
            (tbl^.tbl_file_sid_tid._3)
  where
    ls n = left n ' ' %. stext
    ri n = right n ' ' %. int

dummyMatch :: PennTree -> Instance -> MatchedInstance
dummyMatch tr0 inst
  = let tr = getADTPennTree tr0
        itr = mkIndexedTree tr
    in
    MatchedInstance

    { _mi_instance = inst
    , _mi_arguments
        = do a <- inst^.inst_arguments
             return MatchedArgument { _ma_argument = a
                                    , _ma_nodes = do n <- a^.arg_terminals
                                                     nd <- maybeToList (findNode n itr)
                                                     let rng = (termRange . snd) nd
                                                         zs = maximalEmbeddedRange itr rng
                                                     return MatchedArgNode { _mn_node = (rng,n), _mn_trees = zs }
                                    }

    }


adjustedLemmaMap :: IntMap Lemma -> PennTree -> IntMap (Lemma,Text)
adjustedLemmaMap lemmamap proptr = IM.fromList
                                 . map replacef
                                 . map (\x->(x^._1,(Lemma (x^._2._2),x^._2._2)))
                                 . toList
                                 . mkIndexedTree $ proptr
  where
    replacef (i,(l0,t)) = fromMaybe (i,(l0,t)) $ do j <- eitherToMaybe (adjustIndexFromTree proptr i)
                                                    l <- IM.lookup j lemmamap
                                                    return (i,(l,t))


formatInst :: Bool  -- ^ show detail?
           -> ((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)
           -> Text
formatInst doesShowDetail (filesidtid,corenlp,proptr,inst,_sense) =
  let args = inst^.inst_arguments
      lemmamap = IM.fromList (map (_2 %~ Lemma) (corenlp^._2))
      -- no more use of CoreNLP result. we directly extract information from PropBank instances.
      -- coretr = corenlp^._1
      -- coretr = proptr
      minst = dummyMatch proptr inst
      nlemmamapfull = adjustedLemmaMap lemmamap proptr
      nlemmamap = fmap (^._1) nlemmamapfull
      verbprops = verbPropertyFromPennTree nlemmamap proptr
      tagged = mkPreAnalysis (IM.toList nlemmamapfull) proptr [] [] -- for the time being
      clausetr = clauseStructure tagged verbprops (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx proptr)) -- coretr))
      l2p = linkID2PhraseNode proptr
      iproptr = mkPennTreeIdx proptr
      mvpmva =  matchVerbPropertyWithRelation tagged verbprops clausetr minst
      mtp = do (vp,_) <- mvpmva
               constructCP tagged vp ^? _Just._1.complement
      argtable0 = mkArgTable iproptr l2p filesidtid args
      argtable1 = zipperArgTable iproptr argtable0
      argtable = fmap f argtable1
        where f :: ATNode (BitreeZipper (Range,ChunkTag) (Int,(POSTag,Text))) -> GRel
              f = phraseNodeType mtp . chooseATNode

      fmtfunc = either (const "") (tokenWord.snd) . getRoot . current
  in (if doesShowDetail
       then    "\n\n\n================================================================\n"
            <> formatIndexTokensFromTree 0 proptr
            <> "\n"
            <> "\n================================================================\n"
            <> prettyPrint 0 proptr
            <> "\n"
            <> T.intercalate "\n" (map (formatVerbProperty fmtfunc) verbprops)
            <> "\n"
       else ""
     )
     <> formatArgTable mvpmva argtable


getArgMapFromRoleMap :: (Text,Text) -> [(SenseID, [(Text,Text)])] -> Maybe [(Text,Text)]
getArgMapFromRoleMap (lma,sense_num) rolemap =
  (^._2) <$> find (\rm -> convertONIDtoSenseID lma sense_num == rm^._1) rolemap


formatStatInst :: Bool
               -> [RoleInstance]
               -> Text
               -> ((Text,Text),Int)
               -> (Maybe Text, [((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)])
               -> Text
formatStatInst doesShowDetail rolemap lma ((sense,sense_num),count) (mdefn,insts) =
  let sensetxt = (sense <> "." <> sense_num)
      margmap = getArgMapFromRoleMap (lma,sense_num) rolemap
      rs n = right n ' ' %. stext
      ri n = right n ' ' %. int
  in    "\n============================================================================\n"
     <> sformat (rs 20 % " : "% ri 6 % " :  " % stext %  "\n") sensetxt  count (fromMaybe "" mdefn)
     <> "============================================================================\n"
     <> maybe "" (formatArgMap False) margmap
     <> (T.intercalate "\n" . map (formatInst doesShowDetail)) insts


printHeader :: (Text,Int) -> IO ()
printHeader (lma,count) = do
  let rs n = right n ' ' %. stext
      ri n = right n ' ' %. int
      headstr = sformat (rs 20 % ":" % ri 6) lma count :: Text

  TIO.putStrLn "\n\n\n\n\n*************************************************************"
  TIO.putStrLn "*************************************************************"
  TIO.putStrLn "****                                                     ****"
  TIO.putStrLn (sformat ("****             " % rs 27 %"             ****") headstr)
  TIO.putStrLn "****                                                     ****"
  TIO.putStrLn "*************************************************************"
  TIO.putStrLn "*************************************************************"


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



updateStatMap :: HashMap (ArgPattern Voice GRel) Int
              -> ((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)
              -> HashMap (ArgPattern Voice GRel) Int
updateStatMap !acc (filesidtid,corenlp,proptr,inst,_sense) =
  let args = inst^.inst_arguments
      iproptr = mkPennTreeIdx proptr
      l2p = linkID2PhraseNode proptr
      minst = dummyMatch proptr inst
      lemmamap = IM.fromList (map (_2 %~ Lemma) (corenlp^._2))
      nlemmamapfull = adjustedLemmaMap lemmamap proptr
      nlemmamap = fmap (^._1) nlemmamapfull
      tagged = mkPreAnalysis (IM.toList nlemmamapfull) proptr [] [] -- for the time being
      verbprops = verbPropertyFromPennTree nlemmamap proptr

      clausetr = clauseStructure tagged verbprops (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx proptr))
      mvpmva = matchVerbPropertyWithRelation tagged verbprops clausetr minst
      mtp = do (vp,_) <- mvpmva
               constructCP tagged vp^? _Just._1.complement
      argtable0 = mkArgTable iproptr l2p filesidtid args
      argtable1 = zipperArgTable iproptr argtable0
      argtable = fmap pnt argtable1
        where pnt :: ATNode (BitreeZipper (Range,ChunkTag) (Int,(POSTag,Text))) -> ATNode GRel
              pnt = fmap (phraseNodeType mtp)
      argpatt = mkArgPattern mtp argtable
  in HM.alter (\case Nothing -> Just 1 ; Just n -> Just (n+1)) argpatt acc



showStat :: Bool                        -- ^ is tab separated format
         -> [RoleInstance]
         -> HashMap Text Inventory
         -> [(Text,Int)]                -- ^ lemmastat
         -> HashMap (Text,Text) [((FilePath,Int,Int),(PennTree,LemmaList),PennTree,Instance,SenseInstance)]
         -> IO ()
showStat isTSV rolemap sensedb lemmastat classified_inst_map = do
  let ls n = left n ' ' %. stext
      ri n = right n ' ' %. int
      lst = HM.toList classified_inst_map
  forM_ lemmastat $ \(lma,f) -> do
    when (not isTSV) $ printHeader (lma,f)
    forM_ (countSenseForLemma lma lst) $ \((sense,sense_num),count) -> do
      let (mdefn,insts) = getDefInst sensedb classified_inst_map (sense,sense_num)
          statmap = foldl' updateStatMap HM.empty insts

          statlst = (sortBy (flip compare `on` snd) . HM.toList) statmap
          sensetxt = sense <> "." <> sense_num
          senseheader = "\n============================================================================\n"
                        <> sformat (ls 10 % " : " % ri 6 % " :  " % stext % "\n") sensetxt count (fromMaybe "" mdefn)
                        <> "============================================================================\n"
      if (not isTSV)
        then do
          TIO.putStrLn senseheader
          let margmap = getArgMapFromRoleMap (lma,sense_num) rolemap
          traverse_ (TIO.putStrLn . formatArgMap True) margmap
          forM_ statlst $ \(patt,n :: Int) -> do
            let str1 = formatArgPatt "voice" patt
            TIO.putStrLn (sformat (stext % "     #count: " % ri 5) str1 n)
        else do
          forM_ statlst $ \(patt, n :: Int) -> do
            TIO.putStrLn $
              sformat ( stext % "\t" % stext % "\t" % stext % "\t"
                      % stext % "\t" % stext % "\t" % stext % "\t"
                      % stext % "\t" % stext % "\t" % int)
                      sense
                      sense_num
                      (maybe "null" (T.pack . show) (patt^.patt_property))
                      (maybe "null" formatGRel (patt^.patt_arg0))
                      (maybe "null" formatGRel (patt^.patt_arg1))
                      (maybe "null" formatGRel (patt^.patt_arg2))
                      (maybe "null" formatGRel (patt^.patt_arg3))
                      (maybe "null" formatGRel (patt^.patt_arg4))
                      n

showStatInst :: Bool
             -> [RoleInstance]
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
        TIO.putStrLn (formatStatInst doesShowDetail rolemap lma x definsts)


showError :: Either String a -> IO ()
showError (Left e)  = print e
showError (Right _) = return ()


readSenseInsts :: FilePath -> IO [SenseInstance]
readSenseInsts sensefile =
  fmap (rights . map parseSenseInst . map T.words . T.lines) (TIO.readFile sensefile)


-- | need parse tree to adjust index (deleting none and hyphen)
--
mergePropSense :: PennTree -> [Instance] -> [SenseInstance] -> [(Int, Maybe Instance, SenseInstance)]
mergePropSense proptr insts senses =
  let nonelist = map fst . filter (isNone.fst.snd) . zip [0..] . toList . getADTPennTree
      adj = adjustIndex (nonelist proptr)
      lst = makeKeyAttrib (^.sinst_token_id) senses
  in map toTuple (joinAttrib (\x -> (either id id . adj) (x^.inst_predicate_id)) insts lst)


process :: LexDataConfig -> (Bool,Bool,Bool) -> HashMap Text Inventory -> [FilePath] -> IO ()
process cfg (statonly,tsv,showdetail) sensedb fps = do
  let parsefiles = filter (\x -> takeExtensions x == ".parse") fps
      propfiles  = filter (\x -> takeExtensions x == ".prop" ) fps
      sensefiles = filter (\x -> takeExtensions x == ".sense") fps
      lst = map (\x -> fromTuple (takeBaseName x,x)) parsefiles
      joiner = joinAttrib takeBaseName

      lst' = mapMaybe (\(i,mf1,mf2,f3) -> (i,,,) <$> mf1 <*> mf2 <*> pure f3)
           . map toTuple $ sensefiles `joiner` (propfiles `joiner` lst)

  matchedpairs <- fmap (concat . catMaybes) $ do
    flip traverse lst' $ \(article,sensefile,_propfile,_parsefile) -> do
      hPutStrLn stderr article
      (mprops :: Maybe [(Int,(((PennTree,Dependency,[(Int,Text)]),PennTree),[Instance]))])
        <- join . eitherToMaybe <$>
             runExceptT (loadMatchArticle (cfg^.cfg_wsj_corenlp_directory) (cfg^.cfg_wsj_directory) article)
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

  rolesetstat <- loadStatistics (cfg^.cfg_statistics)
  let lemmastat = mergeStatPB2Lemma rolesetstat
  rolemap <- loadRoleInsts (cfg^.cfg_rolemap_file)

  if statonly
    then showStat tsv rolemap sensedb lemmastat classified_inst_map
    else showStatInst showdetail rolemap sensedb lemmastat classified_inst_map
