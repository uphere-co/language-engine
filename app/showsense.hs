{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import qualified Data.Attoparsec.Text         as A
import           Data.Discrimination                  (joining)
import           Data.Discrimination.Grouping
import           Data.Either.Extra
import           Data.Foldable                        (toList,traverse_)
import           Data.Function                        (on)
import           Data.List                            (intercalate,lookup,sort,sortBy,groupBy)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                            (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T.IO
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.Printf
--
import           Data.Attribute
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           PropBank.Match
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
--
import           OntoNotes.Parser.Sense


parseSenseFile :: FilePath -> IO (Either String [SenseInstance])
parseSenseFile fp = do
  txt <- T.IO.readFile fp
  let lst = T.lines txt
      wss = map T.words lst
  return (traverse parseSenseInst wss)


elookup str k = hoistEither.maybeToEither str.lookup k


formatSense sentterms inst = do
  let n = inst^.sinst_sentence_id
      k = inst^.sinst_token_id
  terms <- elookup ("no such sentence: " ++ show n) n sentterms
  token <- elookup ("no such token: " ++ show k) k terms
  liftIO $ print token
  liftIO $ print inst
  

format1 (_,msinst,pinst) = (fmap f msinst,g pinst)
  where f sinst = sinst^.sinst_sense <> ":" <> T.pack (show (sinst^.sinst_sense_num))
        g pinst = let (lemma,num) = pinst^.inst_lemma_roleset_id
                  in lemma <> "." <> num


format2 :: (Int,Maybe [((Int,Int),Maybe SenseInstance,Instance)],[(Int,Text)]) -> String
format2 (n,mlst,tokens) = printf "sentence %2d: %s\n" n (T.intercalate " " (map (^._2) tokens)) ++
                          "--------------------------------------------------------\n" ++
                          "                    word        propbank           sense\n" ++
                          fromMaybe "" (fmap (format' tokens) mlst) ++
                          "\n--------------------------------------------------------"
  

format' :: [(Int,Text)] -> [((Int,Int),Maybe SenseInstance,Instance)] -> String
format' tokens lst = intercalate "\n" (map f lst)
  where f ((i,j),msinst,pinst) = printf "token %2d %15s %15s %15s" j (fromMaybe "" (lookup j tokens)) fProp fSense
          where fSense = maybe "" (\sinst->sinst^.sinst_sense <> ":" <> T.pack (show (sinst^.sinst_sense_num))) msinst
                fProp = let (lemma,num) = pinst^.inst_lemma_roleset_id
                        in lemma <> "." <> num
       

getFileTriples :: FilePath -> IO [(FilePath,FilePath,FilePath)]
getFileTriples basedir = do
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".parse") fps
      sensefiles = filter (\x -> takeExtensions x == ".sense") fps
      propfiles  = filter (\x -> takeExtensions x == ".prop" ) fps 
      joinf = joinAttrib takeBaseName
      lst :: [(String,Maybe FilePath,Maybe FilePath,FilePath)]
      lst = map toTuple (propfiles `joinf` (sensefiles `joinf` map (fromTuple . (\x ->  (takeBaseName x,x))) parsefiles))
  return $ mapMaybe (\(_,mf1,mf2,f3)->(,,)<$>mf1<*>mf2<*>pure f3) lst
  

readPropBank propfile = liftIO $ parsePropWithFileField NoOmit <$> T.IO.readFile propfile


readPennTree pennfile = hoistEither . A.parseOnly (A.many1 (A.skipSpace *> pnode)) =<< liftIO (T.IO.readFile pennfile)


readSense :: FilePath -> EitherT String IO [SenseInstance]
readSense sensefile =  EitherT $ parseSenseFile sensefile


makeTermListNoNone :: [PennTree] -> [(Int,[(Int,Text)])]
makeTermListNoNone
  = zip [0..] . map (zip [0..] . map snd . filter (\(t,_) -> t /= "-NONE-") . toList)


indexProp :: [PennTree] -> Instance -> Maybe ((Int,Int),Instance)
indexProp trs inst = do
  let i = inst^.inst_tree_id
  tr <- lookup i (zip [0..] trs)
  let adjf = adjustIndexFromTree tr
  case adjf (inst^.inst_predicate_id) of
    Left _ -> Nothing
    Right j -> Just ((i,j),inst)


main :: IO ()
main = do
  let basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj/23"
  lst <- getFileTriples basedir 
  flip traverse_ lst $ \(fp_prop,fp_sense,fp_parse) -> do
    r <- runEitherT $ do
      liftIO $ putStrLn "\n\n\n=============================================================================================="
      liftIO $ putStrLn (fp_sense)
      liftIO $ putStrLn "=============================================================================================="
      propinsts  <- readPropBank  fp_prop
      trs        <- readPennTree fp_parse
      senseinsts <- readSense fp_sense

      let sentterms = makeTermListNoNone trs
      -- traverse_ (T.IO.putStrLn . prettyPrint 0) trs
      {- traverse_ (liftIO . print) sentterms
      traverse_ (formatSense sentterms) senseinsts
      traverse_ (liftIO . print) propinsts -}

      let a_propinsts = map fromTuple (mapMaybe (indexProp trs) propinsts)
          a_sentterms = map fromTuple sentterms
          getidx sinst = (sinst^.sinst_sentence_id,sinst^.sinst_token_id)
          matched1 = groupBy ((==) `on` (^._1._1))
                   . sortBy (compare `on` (^._1))
                   . map toTuple
                   $ joinAttrib getidx senseinsts a_propinsts
          matched2 = map toTuple (joinAttrib ((^._1._1).head) matched1 a_sentterms)
      liftIO $ mapM_ (putStrLn . format2) matched2
    case r of
      Left err -> error err
      Right () -> return ()
