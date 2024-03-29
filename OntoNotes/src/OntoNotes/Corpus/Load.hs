{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OntoNotes.Corpus.Load where

import           Control.Error.Util                 (hoistEither)
import           Control.Exception
import           Control.Lens                hiding ((<.>))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except         (ExceptT(..))
import           Data.Aeson                  hiding (pairs)
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
import           NLP.Parser.PennTreebankII
import           NLP.Type.CoreNLP
import           NLP.Type.PennTreebankII
import           NLP.Type.PennTreebankII.Match
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
import           PropBank.Util
--
import           OntoNotes.Parser.Sense
import           OntoNotes.Type.Sense


readJSONList :: (FromJSON a) => FilePath -> ExceptT String IO [a]
readJSONList f = ExceptT $ eitherDecode <$> liftIO (BL.readFile f)


readPropBank :: FilePath -> ExceptT String IO [Instance]
readPropBank propfile = liftIO $ parsePropWithFileField NoOmit <$> T.IO.readFile propfile


readOrigPennTree :: FilePath -> ExceptT String IO [PennTree]
readOrigPennTree pennfile
  = hoistEither . A.parseOnly (A.many1 (A.skipSpace *> pnode))
    =<< liftIO (T.IO.readFile pennfile)


prepareFilePaths :: FilePath -> IO ([FilePath],[FilePath])
prepareFilePaths basedir = do
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      props = filter (\x -> takeExtensions x == ".prop") fps
      trees = filter (\x -> takeExtensions x == ".parse") fps
  return (props,trees)


loadMatchArticle :: FilePath -> FilePath -> String
                 -> ExceptT String IO (Maybe [(Int,(((PennTree,Dependency,[(Int,Text)]),PennTree),[Instance]))])
loadMatchArticle ptreedir basedir article = do
  (props,trees) <- liftIO $ prepareFilePaths basedir
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


errorHandler :: Handle -> String -> IO a -> IO ()
errorHandler h_err msg action = do
  r <- try action
  case r of
    Left (_ :: SomeException) -> hPutStrLn h_err msg >> hFlush h_err
    _ -> return ()

senseInstStatistics :: FilePath -> IO (HashMap (Text,Text) Int)
senseInstStatistics basedir = do
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      sfiles = filter (\x -> takeExtensions x == ".sense") fps

  sinstss <- flip mapM sfiles $ \fp -> do
    txt <- T.IO.readFile fp
    let lst = T.lines txt
        wss = map T.words lst
    case traverse parseSenseInst wss of
      Left  e -> error e
      Right r -> return r

  let sinsts = concat sinstss
      sinsts_verb = filter (\s-> T.last (s^.sinst_sense) == 'v') sinsts
      ks = map (\s -> ( T.init (T.init (s^.sinst_sense)) ,s^.sinst_sense_num)) sinsts_verb
  return (foldl' (\(!acc) k -> HM.insertWith (+) k 1 acc) HM.empty ks)


mergeStatPB2Lemma :: [(Text,Text)] -> [(Text,Int)]
mergeStatPB2Lemma ws =
  let count :: [(Text,Text)] -> (Text,Int)
      count lst = let (lma,_) = head lst
                  in case mapM (decimal.snd) lst of
                       Left _     -> (lma,0)
                       Right lst' -> (lma,sum (map fst lst'))

  in sortBy (flip compare `on` snd) . map count . groupBy ((==) `on` fst)
     . sortBy (flip compare `on` fst)
     . map (\(l,f)-> let (lma,_) = T.break (== '.') l in (lma,f))
     $ ws
