{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module SRL.Train where

import           AI.SVM.Simple
import           AI.SVM.Base
import           Control.Exception
import           Control.Lens               hiding (levels,(<.>))
import           Control.Monad                     (void,when)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import           Data.Default
import           Data.Foldable                     (toList)
import           Data.Function                     (on)
import           Data.List                         (group,sort,sortBy,zip4)
import           Data.Maybe                        (fromJust,mapMaybe)
import           Data.Monoid                       ((<>),Monoid(..))
import qualified Data.Sequence              as Seq
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Vector.Storable              (Vector)
import qualified Data.Vector.Storable       as V
import           Foreign.C.Types
import           Language.Java              as J
import           System.FilePath                   ((</>),(<.>))
import           Text.Printf
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           FastText.Binding
import           NLP.Type.PennTreebankII
import           PropBank.Type.Prop
import           PropBank.Util
--
import           SRL.Feature 
import           SRL.PropBankMatch
import           SRL.Type
import           SRL.Vectorize


data SVMFarm = SVMFarm { _svm_arg0 :: SVM
                       , _svm_arg1 :: SVM
                       }
               
makeLenses ''SVMFarm

data TrainingData = TrainingData { _training_arg0 :: [(Double,Vector Double)]
                                 , _training_arg1 :: [(Double,Vector Double)]
                                 }

makeLenses ''TrainingData                                              

instance Monoid TrainingData where
  mempty = TrainingData [] []
  t1 `mappend` t2 = TrainingData (t1^.training_arg0 ++ t2^.training_arg0) (t1^.training_arg1 ++ t2^.training_arg1)


getIdxSentProps pp (trs,props) = do
    rdocs <- liftIO $ do
      let docs = map mkDocFromPennTree trs
      anns <- mapM (annotate pp) docs
      rdocs <- mapM protobufDoc anns
      return rdocs
    ds <- mapM hoistEither rdocs
    let sents = map (flip Seq.index 0 . (^. D.sentence)) ds
    deps <- hoistEither $ mapM sentToDep sents
    let cpts = mapMaybe (^.S.parseTree) sents
        pts = map decodeToPennTree cpts
        rs = map (\(i,((pt,tr,dep,sent),pr)) -> (i,SentInfo sent pt dep,tr,pr))
           . merge (^.inst_tree_id) (zip4 pts trs deps sents)
           $ props
    return rs

header fp = do
  putStrLn "*****************************"
  putStrLn "*****************************"
  putStrLn "*****************************"
  putStrLn "*****************************"
  putStrLn fp 
  putStrLn "*****************************"
  putStrLn "*****************************"
  putStrLn "*****************************"
  putStrLn "*****************************"

      
trainingVectorsForArg ft arglabel (ifeats,ifakefeats) = do
  ts <- concat <$> mapM (inst2vec ft) ifeats
  let ts' = filter ((== arglabel) . (^._3)) ts 
      ts'' = map (\x -> (1 :: Double,x^._5)) ts'
  fs <- concat <$> mapM (inst2vec ft) ifakefeats
  let fs' = filter ((== arglabel) . (^._3)) fs
      fs'' = map (\x -> (-1 :: Double,x^._5)) fs'
  return $ map (\(t,v) -> (t,V.map realToFrac v)) (ts''++fs'')


trainingFarmPerFile ft rs = do
  rs <- flip mapM rs $ \(_,sentinfo,propbanktree,prs) -> do
    let ifeats = features (sentinfo,propbanktree,prs)
        ifakefeats = fakeFeatures (sentinfo,propbanktree,prs)
    dat0 <- trainingVectorsForArg ft (NumberedArgument 0) (ifeats,ifakefeats)
    dat1 <- trainingVectorsForArg ft (NumberedArgument 1) (ifeats,ifakefeats)
    return (TrainingData dat0 dat1)
  return (mconcat rs)

prepareTraining :: FastText
                -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                -> (FilePath,FilePath)
                -> (FilePath,IsOmit)
                -> EitherT String IO TrainingData
prepareTraining ft pp (dirpenn,dirprop) (fp,omit) = do
  let pennfile = dirpenn </> fp <.> "mrg"
      propfile = dirprop </> fp <.> "prop"
  (trs,props) <- propbank (pennfile,propfile,omit)
  rs <- getIdxSentProps pp (trs,props)
  liftIO $ mapM_ (showMatchedInstance <> showFeatures <> showFakeFeatures) rs
  liftIO (trainingFarmPerFile ft rs)  




classifyFile ft pp svm (dirpenn,dirprop) (fp,omit) = do
  runEitherT $ do
    liftIO $ print fp
    (trs,props) <- propbank (dirpenn </> fp <.> "mrg" ,dirprop </> fp <.> "prop", omit)
    runsvm ft pp svm (trs,props) 



groupFeatures (i,roleset,voice,afeatss_t) (i',_,_,afeatss_f) = 
  if i /= i'
  then error "error in groupFeatues" 
  else (i,roleset,voice,zipWith (++) afeatss_t afeatss_f)


findArgument arglabel ft svmfarm ifeat = do
  let svm = if | arglabel == NumberedArgument 0 -> svmfarm^.svm_arg0
               | arglabel == NumberedArgument 1 -> svmfarm^.svm_arg1
               | otherwise                      -> error "only arg0 and arg1 are supported"
  ts <- liftIO (inst2vec ft ifeat)
  let ts' = filter (\x -> x^._3 == arglabel) ts
      ts_v = map (V.map realToFrac . (^._5)) ts'
      ts_result = map (predict svm) (ts_v :: [Vector Double])
      ts'' = zipWith (\x r -> (_5 .~ r) x) ts' ts_result
  return ts'' 

runsvm ft pp svm (trs,props) = do
  rs <- getIdxSentProps pp (trs,props)
  flip mapM_ rs $ \(i,sentinfo,propbanktree,pr) -> do
    let ifeats = features (sentinfo,propbanktree,pr)
        ifakefeats = fakeFeatures (sentinfo,propbanktree,pr)
        sortFun = sortBy (flip compare `on` (^._5))
        feats = zipWith groupFeatures ifeats ifakefeats
    resultss0 <- mapM (fmap sortFun . findArgument (NumberedArgument 0) ft svm) feats
    resultss1 <- mapM (fmap sortFun . findArgument (NumberedArgument 1) ft svm) feats
    let results = sortBy (compare `on` (^._1)) . map (\x -> head x) . filter (not.null) $ resultss0 ++ resultss1
    let pt = sentinfo^.corenlp_tree
        ipt = mkPennTreeIdx pt
        terms = map (^._2) . toList $ pt
    liftIO $ putStrLn "======================================================================================="        
    liftIO $ TIO.putStrLn (T.intercalate " " terms)
    liftIO $ putStrLn "======================================================================================="
    liftIO $ flip mapM_ results $ \result -> do
      let mmatched = matchR (result^._4) ipt
      case mmatched of
        Nothing -> TIO.putStrLn "no matched?"
        Just matched -> let txt = T.intercalate " " (map (^._2._2) (toList matched))
                        in putStrLn $ formatResult result txt

formatResult (n,(lemma,sensenum),label,range,value) txt =
  printf "%d %15s.%2s %8s %8s %8.5f %s" n lemma sensenum (pbLabelText label) (show range) value txt
    

train ft pp (dirpenn,dirprop) trainingFiles = do
  lsts <- flip mapM trainingFiles $ \(fp,omit) -> do
    r <- try $ do 
      header fp
      runEitherT $ prepareTraining ft pp (dirpenn,dirprop) (fp,omit)
    case r of
      Left (e :: SomeException) -> error $ "In " ++ fp ++ " exception : " ++ show e 
      Right (Right lst) -> return lst
      Right (Left e) -> error ("in run: " ++ e)
  let trainingData = mconcat lsts
  
  (msg0,svm0) <- trainSVM (EPSILON_SVR 1 0.1) (RBF 1) [] (trainingData ^.training_arg0)
  (msg1,svm1) <- trainSVM (EPSILON_SVR 1 0.1) (RBF 1) [] (trainingData ^.training_arg1)

  return (SVMFarm svm0 svm1)

preparePP :: IO (J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline"))
preparePP = do
  let pcfg = def & ( tokenizer .~ True )
                 . ( words2sentences .~ True )
                 . ( postagger .~ True )
                 . ( lemma .~ True )
                 . ( sutime .~ False )
                 . ( depparse .~ True )
                 . ( constituency .~ True )
                 . ( ner .~ False )
  prepare pcfg


