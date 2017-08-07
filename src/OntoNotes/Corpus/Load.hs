{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OntoNotes.Corpus.Load where

import           Control.Exception
import           Control.Lens                hiding ((<.>))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.Attoparsec.Text       as A
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable
import           Data.Function                      (on)
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.Text                          (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Data.Text.Read                     (decimal)
import           System.Directory.Tree
import           System.FilePath
import           System.IO
--
import           CoreNLP.Simple.Type.Simplified
import           NLP.Parser.PennTreebankII
import           NLP.Type.PennTreebankII
import           PropBank.Match
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
import           PropBank.Util
--
import           OntoNotes.Parser.Sense
import           OntoNotes.Type.Sense


readJSONList :: (FromJSON a) => FilePath -> EitherT String IO [a]
readJSONList file = EitherT $ eitherDecode <$> liftIO (BL.readFile file)


readPropBank propfile = liftIO $ parsePropWithFileField NoOmit <$> T.IO.readFile propfile


readOrigPennTree pennfile
  = hoistEither . A.parseOnly (A.many1 (A.skipSpace *> pnode))
    =<< liftIO (T.IO.readFile pennfile)


prepare basedir = do
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      props = filter (\x -> takeExtensions x == ".prop") fps
      trees = filter (\x -> takeExtensions x == ".parse") fps
  return (props,trees)


loadMatchArticle :: FilePath -> FilePath -> String
                 -> EitherT String IO (Maybe [(Int,(((PennTree,Dependency,[(Int,Text)]),PennTree),[Instance]))])
loadMatchArticle ptreedir basedir article = do
  (props,trees) <- liftIO $ prepare basedir
  let findf = find (\f -> takeBaseName f == article)
  flip traverse ((,) <$> findf props <*> findf trees) $ \(fprop,ftree) -> do
    insts <- readPropBank fprop
    proptrs' <- readOrigPennTree ftree
    let proptrs = map convertTop proptrs'
        ptreefile = article <.> "corenlp_ptree"
        depfile = article <.> "corenlp_udep"
        plemmafile = article <.> "corenlp_lemma"
    coretrs :: [PennTree]  <- readJSONList  (ptreedir </> ptreefile)
    coredeps :: [Dependency] <- readJSONList (ptreedir </> depfile)
    corelmas :: [[(Int,Text)]] <- readJSONList (ptreedir </> plemmafile)
    let cores = zip3 coretrs coredeps corelmas
        pairs = zip cores proptrs
    return (merge (^.inst_tree_id) pairs insts)



errorHandler h_err msg action = do
  r <- try action
  case r of
    Left (e :: SomeException) -> hPutStrLn h_err msg >> hFlush h_err
    _ -> return ()

senseInstStatistics :: FilePath -> IO (HashMap (Text,Text) Int)
senseInstStatistics basedir = do
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      sfiles = filter (\x -> takeExtensions x == ".sense") fps

  sinstss <- flip mapM sfiles $ \fp -> do
    txt <- T.IO.readFile fp
    -- print fp
    let lst = T.lines txt
        wss = map T.words lst
    case traverse parseSenseInst wss of
      Left err -> error err
      Right lst -> return lst

  let sinsts = concat sinstss
      sinsts_verb = filter (\s-> T.last (s^.sinst_sense) == 'v') sinsts
      ks = map (\s -> ( T.init (T.init (s^.sinst_sense)) ,s^.sinst_sense_num)) sinsts_verb
      acc = foldl' (\(!acc) k -> HM.insertWith (+) k 1 acc) HM.empty ks
  -- mapM_ (putStrLn.formatStat) . sortBy (flip compare `on` snd) . HM.toList $ acc
  return acc

mergeStatPB2Lemma :: [(Text,Text)] -> [(Text,Int)]
mergeStatPB2Lemma ws =
  let merge :: [(Text,Text)] -> (Text,Int)
      merge lst = let (lma,_) = head lst
                  in case mapM (decimal.snd) lst of
                       Left _     -> (lma,0)
                       Right lst' -> (lma,sum (map fst lst'))

  in sortBy (flip compare `on` snd) . map merge . groupBy ((==) `on` fst)
     . sortBy (flip compare `on` fst)
     . map (\(l,f)-> let (lma,_) = T.break (== '.') l in (lma,f))
     $ ws

