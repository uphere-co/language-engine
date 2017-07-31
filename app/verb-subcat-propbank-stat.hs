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
import           OntoNotes.Corpus.Load
import           OntoNotes.Corpus.PropBank
import           OntoNotes.Parser.Sense


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
                         , _tbl_arg4 :: Maybe Text
                         , _tbl_file_sentid :: (FilePath,Int)
                         } 

makeLenses ''ArgTable


headPreposition :: [PennTreeIdx] -> Maybe Text
headPreposition xs = getFirst (foldMap (First . f) xs)   
  where f (PN _ _)        = Nothing
        f (PL (_,(IN,t))) = Just t
        f (PL (_,(TO,t))) = Just t
        f (PL (_,(_,t)))  = Nothing        


phraseNodeType (PN (_,c) xs) = case c of
                                 PP   -> T.pack (show c) <> maybe "" (\t -> "-" <> t) (headPreposition xs)
                                 WHNP -> "NP"
                                 _     -> T.pack (show c)
phraseNodeType (PL (_,(D_NONE,t))) = t
phraseNodeType (PL (_,(p     ,t))) = case isNoun p of
                                       Yes -> "NP"
                                       _   -> "??" <> T.pack (show (p,t))


mkArgTable :: PennTreeIdx -> [(LinkID,Range)] -> (FilePath,Int) -> [Argument] -> ArgTable
mkArgTable itr l2p (file,sentid) args  =
    ArgTable (T.intercalate " " . map (^._2._2) . toList <$> (findArg (== Relation)))
             (phraseNodeType . adj <$> findArg (== NumberedArgument 0))
             (phraseNodeType . adj <$> findArg (== NumberedArgument 1))
             (phraseNodeType . adj <$> findArg (== NumberedArgument 2))
             (phraseNodeType . adj <$> findArg (== NumberedArgument 3))
             (phraseNodeType . adj <$> findArg (== NumberedArgument 4))
             (file,sentid)
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

formatArgTable mvpmva tbl = printf "%-15s (%-10s)  arg0: %-10s   arg1: %-10s   arg2: %-10s   arg3: %-10s   arg4: %-10s            ## %10s sentence %3d"
                              (fromMaybe "" (tbl^.tbl_rel))
                              (maybe "unmatched" (\(vp,_) -> show (vp^.vp_voice)) mvpmva)
                              (fromMaybe "" (tbl^.tbl_arg0))
                              (fromMaybe "" (tbl^.tbl_arg1))
                              (fromMaybe "" (tbl^.tbl_arg2))
                              (fromMaybe "" (tbl^.tbl_arg3))
                              (fromMaybe "" (tbl^.tbl_arg4))
                              (tbl^.tbl_file_sentid._1)
                              (tbl^.tbl_file_sentid._2)


formatInst :: Bool  -- ^ show detail?
           -> ((FilePath,Int),(PennTree,LemmaList),PennTree,Instance)
           -> String
formatInst doesShowDetail (filesentid,corenlp,proptr,inst) =
  let args = inst^.inst_arguments
      lmap = IM.fromList (map (_2 %~ Lemma) (corenlp^._2))
      coretr = corenlp^._1
      
      minst = MatchedInstance { _mi_instance = inst, _mi_arguments = matchArgs (coretr,proptr) inst }
      
      verbprops = verbPropertyFromPennTree lmap coretr 
      clausetr = clauseStructure verbprops (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx coretr))
      l2p = linkID2PhraseNode proptr
      iproptr = mkPennTreeIdx proptr
      argtable = mkArgTable iproptr l2p filesentid args
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
     

formatStatInst :: Bool           -- ^ show detail?
               -> PredicateDB
               -> HashMap RoleSetID [((FilePath,Int),(PennTree,LemmaList),PennTree,Instance)]
               -> (RoleSetID,Int)
               -> String
formatStatInst doesShowDetail db imap (rid,num) =
  let mdefn = lookupRoleset db rid
      minsts = HM.lookup rid imap
  in
     "\n\n\n============================================================================\n"
     ++ printf "%20s : %5d : %s\n" (formatRoleSetID rid) num  (fromMaybe "" mdefn)
     ++ "============================================================================\n"
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


data Config = Config { _propframedir :: FilePath
                     , _corenlpdir   :: FilePath
                     , _basedir      :: FilePath
                     }

makeLenses ''Config
           
config = Config { _propframedir = "/home/wavewave/repo/srcc/propbank-frames/frames"
                , _corenlpdir   = "/scratch/wavewave/run/ontonotes_corenlp_ptree_udep_lemma_20170710"
                , _basedir      = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
                }


main' = do
  opt <- execParser progOption
  
  propdb <- constructFrameDB (config^.propframedir)
  let preddb = constructPredicateDB propdb

  dtr <- build (config^.basedir)
  let fps = Prelude.take 20 $ sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".prop") fps
      
  parsedpairs <- fmap (concat . catMaybes) $ do
    flip traverse parsefiles $ \f -> do
      let article  = takeBaseName f
      hPutStrLn stderr article
      (o :: Maybe [(Int,(_,_))]) <- join . eitherToMaybe <$> runEitherT (loadMatchArticle (config^.corenlpdir) (config^.basedir) article)
      return . fmap (map (_1 %~ \i -> (article,i))) $ o
      
  let flatParsedPairs = do (filesentid,(((coretr,_,corelma),proptr),insts)) <- parsedpairs
                           inst <- insts
                           return (filesentid,(coretr,corelma),proptr,inst)
  let insts_v = filter (\p->T.last (p^._4.inst_lemma_type) == 'v') flatParsedPairs
      classified_inst_map = foldl' addfunc  HM.empty insts_v
          where addfunc acc x = HM.insertWith (++) (x^._4.inst_lemma_roleset_id) [x] acc
  showStatInst (showDetail opt) preddb classified_inst_map


readSenseInsts sensefile = fmap (rights . map parseSenseInst . map T.words . T.lines) (T.IO.readFile sensefile)


  
main = do
  dtr <- build (config^.basedir)
  let fps = Prelude.take 20 $ sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".parse") fps
      propfiles  = filter (\x -> takeExtensions x == ".prop" ) fps
      sensefiles = filter (\x -> takeExtensions x == ".sense") fps
      lst = map (\x -> fromTuple (takeBaseName x,x)) parsefiles
      joiner = joinAttrib takeBaseName 
     
      lst' = mapMaybe (\(i,mf1,mf2,f3) -> (i,,,) <$> mf1 <*> mf2 <*> pure f3)
           . map toTuple $ sensefiles `joiner` (propfiles `joiner` lst)

  mapM_ print lst'
  
  flip mapM_ lst' $ \(article,sensefile,propfile,parsefile) -> do
    (mprops :: Maybe [(Int,(_,_))]) <- join . eitherToMaybe <$> runEitherT (loadMatchArticle (config^.corenlpdir) (config^.basedir) article)
    senses <- readSenseInsts sensefile
    case mprops of
      Nothing -> return ()
      Just props -> do
        let props' = do (i,(((coretr,_,corelma),proptr),insts)) <- props
                        inst <- insts
                        let predid = inst^.inst_predicate_id
                        return ((i,predid),(coretr,corelma),proptr,inst)
        
        -- let props' = map (\x -> (x^._1,x^,_2)
        let merged = filter (\x -> isJust (x^._2)) . map toTuple $
                       joinAttrib (\x->(x^.sinst_sentence_id,x^.sinst_token_id)) senses (map fromTuple props')
        mapM_ print merged
