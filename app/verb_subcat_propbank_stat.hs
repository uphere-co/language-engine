{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Attoparsec.Text  as A  hiding (try)
import           Data.Foldable
import           Data.Function                (on)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict   as HM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T.IO
import           Data.Traversable
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.Printf
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           PropBank.Format
import           PropBank.Parser.Prop
import           PropBank.Match
import           PropBank.Query
import           PropBank.Type.Frame
import           PropBank.Type.Prop
import           PropBank.Util                (merge)
--
import           OntoNotes.Corpus.Load


lookupRoleset :: PredicateDB -> (Text,Text) -> Maybe Text
lookupRoleset db (lma,sens) = do
  p <- HM.lookup lma (db^.predicateDB)
  let rs = p ^.predicate_roleset
  defn <- Data.List.lookup (lma <> "." <> sens) $ map (\r -> (r^.roleset_id,fromMaybe "" (r^.roleset_name))) rs
  return defn

maybeNumberedArgument :: PropBankLabel -> Maybe Int
maybeNumberedArgument (NumberedArgument n) = Just n
maybeNumberedArgument _                    = Nothing



formatArgNodes :: PennTree -> Argument -> Text
formatArgNodes tr arg =
  let nodes = arg^.arg_terminals
  in T.intercalate "\n" . map (T.pack . show . snd) . mapMaybe (\n -> findNode n tr) $  nodes


formatInst :: (Int,PennTree,Instance) -> String
formatInst (_,tr,inst) =
  let args = inst^.inst_arguments
      
  in "\n================================================================\n" ++
     (T.unpack . T.intercalate " " . map snd . toList) tr                   ++
     "\n================================================================\n" ++
     (intercalate "\n--------------------------------------------------------------\n" $ flip map args $ \arg ->
          show (arg^.arg_label) ++ "\n" ++ (T.unpack (formatArgNodes tr arg))
     )
       
   --  T.unpack (prettyPrint 0 tr) ++ "\n" ++ show args




formatStatInst :: PredicateDB
               -> HashMap RoleSetID [(Int,PennTree,Instance)]
               -> (RoleSetID,Int)
               -> String
formatStatInst db imap (rid,num) =
  let mdefn = lookupRoleset db rid
      minsts = HM.lookup rid imap
  in printf "%20s : %5d : %s\n" (formatRoleSetID rid) num  (fromMaybe "" mdefn)
     ++ (intercalate "\n" . map formatInst . concat . maybeToList) minsts


showStatInst preddb classified_inst_map = do
  let lst = HM.toList classified_inst_map
      stat = sortBy (flip compare `on` (^._2)) $ map (_2 %~ length) lst
      -- sorted = sortBy (flip compare `on` (^._2)) stat
  mapM_ (putStrLn . formatStatInst preddb classified_inst_map) stat


errorHandler h_err msg action = do
  r <- try action
  case r of
    Left (e :: SomeException) -> hPutStrLn h_err msg >> hFlush h_err
    _ -> return ()

showError (Left err) = print err
showError (Right _) = return ()

main = do
  let propframedir = "/home/wavewave/repo/srcc/propbank-frames/frames" 
  propdb <- constructFrameDB propframedir
  let preddb = constructPredicateDB propdb

  let corenlpdir = "/scratch/wavewave/run/ontonotes_corenlp_ptree_udep_lemma_20170710"
      basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"

  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".prop") fps
{- 
  -- let fps = Prelude.take 100 $ sort (toList (dirTree dtr))
      props = filter (\x -> takeExtensions x == ".prop") fps
      trees = filter (\x -> takeExtensions x == ".parse") fps
      pairs = flip map props $ \fp -> 
                let article = takeBaseName fp
                    findf = find (\f -> takeBaseName f == article)
                in (,) <$> findf props <*> findf trees -}
  (showError <=< runEitherT) $ do
    parsedpairs
      <- fmap (concat . catMaybes) <$> flip traverse parsefiles $ \f -> do
           let article  = takeBaseName f
           liftIO $ print article
           loadMatchArticle corenlpdir basedir article
    liftIO $ print parsedpairs
    {- 
    parsedpairs <- flip traverse (catMaybes pairs) $ \(fprop,ftree) -> do
    
      liftIO $ hPutStrLn stderr fprop
      insts <- readPropBank fprop
      proptrs' <- readOrigPennTree ftree
      let proptrs = map convertTop proptrs'
      return (merge (^.inst_tree_id) proptrs insts)
    -}
    let flatParsedPairs = do (i,(((_,_,_),tr),insts)) <- parsedpairs
                             inst <- insts
                             return (i,tr,inst)
    let insts_v = filter (\p->T.last (p^._3.inst_lemma_type) == 'v') flatParsedPairs
        -- insts_v = filter (\x -> x^._3.inst_lemma_roleset_id._1 == "call") all_insts_v
        classified_inst_map = foldl' addfunc  HM.empty insts_v
            where addfunc acc x = HM.insertWith (++) (x^._3.inst_lemma_roleset_id) [x] acc
        {- rolesets = map (^.inst_lemma_roleset_id) insts_v
        stat = foldl' (flip (HM.alter (\case { Nothing -> Just 1; Just n -> Just (n+1)}))) HM.empty rolesets
        -}
    liftIO $ showStatInst preddb classified_inst_map

-- ) . sortBy (flip compare `on` snd) . HM.toList $ stat


  -- mapM_ (putStrLn . formatStat preddb) . sortBy (flip compare `on` snd) . HM.toList $ acc
