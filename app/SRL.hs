{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where


import           AI.SVM.Base
import           Control.Applicative               (optional)
import           Control.Lens               hiding (levels,(<.>))
import           Control.Monad                     (void,when)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Char8      as B
import           Data.Foldable                     (toList)
import           Data.Monoid                       ((<>))
import           Data.Maybe                        (fromMaybe,mapMaybe)
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar                (fromGregorian)
import           Foreign.JNI                as J
import           Language.Java              as J
import           Options.Applicative
import           System.Environment                (getEnv)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
-- import           FastText.Binding
import           NLP.Type.PennTreebankII
import           PropBank.Type.Prop
--
import           SRL.DataSet.PropBank
import           SRL.Feature
import           SRL.Train
import           SRL.Type


data ProgOption = ProgOption { penndir :: Maybe FilePath
                             , propdir :: Maybe FilePath
                             , isTraining :: Bool
                             , isTesting :: Bool
                             , svmFile0 :: Maybe FilePath
                             , svmFile1 :: Maybe FilePath
                             , textFile :: Maybe FilePath
                             } deriving Show


pOptions :: Parser ProgOption
pOptions = ProgOption <$> optional (strOption (long "penn" <> short 'n' <> help "Penn Treebank directory"))
                      <*> optional (strOption (long "prop" <> short 'p' <> help "PropBank directory"))
                      <*> switch (long "train" <> help "is training")
                      <*> switch (long "test" <> help "is testing")
                      <*> optional (strOption (long "arg0" <> help "SVM file for arg0"))
                      <*> optional (strOption (long "arg1" <> help "SVM file for arg1"))
                      <*> optional (strOption (long "file" <> help "text file"))


progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "Test features for PropBank corpus")


initGHCi :: IO J.JVM
initGHCi = do
  clspath <- getEnv "CLASSPATH"
  J.newJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] 

-- init2 :: IO FastText
-- init2 = initWVDB "/scratch/wavewave/wordvector/wiki.en.bin"

main :: IO ()
main = do
  opt <- execParser progOption
  let (trainingFiles,testFiles) = splitAt 50 propbankFiles
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    when (isTraining opt) $ do
      case (penndir opt,propdir opt) of
        (Just dirpenn,Just dirprop) -> do
          -- ft <- init2
          pp <- preparePP 
          let dirs = (dirpenn,dirprop)
          svmfarm <- train pp dirs trainingFiles   
          saveSVM "arg0.svm" (svmfarm^.svm_arg0)
          saveSVM "arg1.svm" (svmfarm^.svm_arg1)
        _ -> error "penn/prop dir are not specified"
    -- 
    let svmfile0 = fromMaybe "arg0.svm" (svmFile0 opt)
        svmfile1 = fromMaybe "arg1.svm" (svmFile1 opt)

        
    if | isTesting opt -> do
           case (penndir opt,propdir opt) of
             (Just dirpenn,Just dirprop) -> do
               -- ft <- init2
               pp <- preparePP 
               let dirs = (dirpenn,dirprop)
               svm0 <- loadSVM svmfile0
               svm1 <- loadSVM svmfile1
               let svmfarm = SVMFarm svm0 svm1
               mapM_ (matchRoleForPBCorpusFile pp svmfarm dirs) testFiles
             _ -> error "penn/prop dir are not specified"
       | isTraining opt -> return ()
       | otherwise      -> do
           case textFile opt of
             Nothing -> error "No text file was specified."
             Just fp -> do
               txt <- TIO.readFile fp
               -- TIO.putStrLn txt
               -- ft <- init2
               pp <- preparePP 
               ann <- annotate pp (Document txt (fromGregorian 2017 4 17))
               rdoc <- protobufDoc ann
               void . runEitherT $ do
                 d <- hoistEither rdoc
                 let sents = d^.. D.sentence . traverse
                 deps <- hoistEither $ mapM sentToDep sents
                 let cpts = mapMaybe (^.S.parseTree) sents
                     pts = map decodeToPennTree cpts
                     rs = zipWith3 SentInfo sents pts deps
                 flip mapM_ rs $ \sentinfo -> do
                   let ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
                       -- terms = map (^._2) . toList $ sentinfo^.corenlp_tree
                       lemmamap = mkLemmaMap (sentinfo^.corenlp_sent)
                       lemmapt = lemmatize lemmamap ipt
                       verbs = filter (isVerb . (^._2._1)) (toList lemmapt)
                       
                   let arg0 = NumberedArgument 0
                       arg1 = NumberedArgument 1
                   svm0 <- liftIO $ loadSVM svmfile0
                   svm1 <- liftIO $ loadSVM svmfile1
                       
                   let svmfarm = SVMFarm svm0 svm1
                       genArgInputs n = let rngss = map (\x->Single x) (findNotOverlappedNodes ipt (n,n))
                                        in [ArgumentInput arg0 rngss, ArgumentInput arg1 rngss]
                       instInputs = flip map verbs $ \verb ->
                                      let n = verb^._1
                                          lma = verb^._2._2._2
                                      in InstanceInput n (lma,"01") (genArgInputs n)
                       feats = map (calcInstanceFeature sentinfo) instInputs
                   matchRole svmfarm sentinfo feats


isVerb :: POSTag -> Bool                     
isVerb VB  = True
isVerb VBZ = True
isVerb VBP = True
isVerb VBD = True
isVerb VBN = True
isVerb VBG = True
isVerb _   = False
