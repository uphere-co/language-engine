{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module SRL.Train where

import           Control.Exception
import           Control.Lens               hiding (levels,(<.>))
import           Control.Monad.IO.Class            (MonadIO(liftIO))
import           Control.Monad.Trans.Either
import           Data.Foldable                     (toList)
import           Data.Function                     (on)
import           Data.List                         (sortBy,zip4)
import           Data.Maybe                        (mapMaybe)
import           Data.Monoid                       ((<>),Monoid(..))
import qualified Data.Sequence              as Seq
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Language.Java              as J
import           System.FilePath                   ((</>),(<.>))
import           Text.Printf
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           NLP.Type.PennTreebankII
import           PropBank.Match
import           PropBank.Type.Prop
import           PropBank.Util
--
import           SRL.Feature 
import           SRL.PropBankMatch
import           SRL.Type
import           SRL.Vectorize.Sparse
import           SVM


data SVMFarm = SVMFarm { _svm_arg0 :: SVM
                       , _svm_arg1 :: SVM
                       }
               
makeLenses ''SVMFarm

data TrainingData = TrainingData { _training_arg0 :: [(Double,[(Int,Double)])]
                                 , _training_arg1 :: [(Double,[(Int,Double)])]
                                 }

makeLenses ''TrainingData                                              

instance Monoid TrainingData where
  mempty = TrainingData [] []
  t1 `mappend` t2 = TrainingData (t1^.training_arg0 ++ t2^.training_arg0) (t1^.training_arg1 ++ t2^.training_arg1)



getIdxSentProps :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                -> ([PennTree],[Instance])
                -> EitherT String IO [(Int,SentenceInfo,PennTree,[Instance])]
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


header :: FilePath -> IO ()
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


formatResult :: (Int,RoleSet,PropBankLabel,Range,Double) -> Text -> String
formatResult (n,(lmma,sensenum),label,range,value) txt =
  printf "%d %15s.%2s %8s %8s %8.5f %s" n lmma sensenum (pbLabelText label) (show range) value txt
    


trainingVectorsForArg :: PropBankLabel
                      -> ([InstanceFeature],[InstanceFeature])
                      -> [(Double,[(Int,Double)])]
trainingVectorsForArg arglabel (ifeats,ifakefeats) = 
  let ts = concatMap inst2vec ifeats
      ts' = filter ((== arglabel) . (^._3)) ts 
      ts'' = map (\x -> (1 :: Double,x^._5.fv_nodes)) ts'
      fs = concatMap inst2vec ifakefeats
      fs' = filter ((== arglabel) . (^._3)) fs
      fs'' = map (\x -> (-1 :: Double,x^._5.fv_nodes)) fs'
  in ts''++fs''


trainingDataPerFile :: [(Int,SentenceInfo,PennTree,[Instance])] -> TrainingData
trainingDataPerFile rs = 
  let results = flip map rs $ \(_,sentinfo,propbanktree,prs) ->
        let ifeats = features (sentinfo,propbanktree,prs)
            ifakefeats = fakeFeatures (sentinfo,propbanktree,prs)
            dat0 = trainingVectorsForArg (NumberedArgument 0) (ifeats,ifakefeats)
            dat1 = trainingVectorsForArg (NumberedArgument 1) (ifeats,ifakefeats)
        in TrainingData dat0 dat1
  in mconcat results


prepareTraining :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                -> (FilePath,FilePath)
                -> (FilePath,IsOmit)
                -> EitherT String IO TrainingData
prepareTraining pp (dirpenn,dirprop) (fp,omit) = do
  let pennfile = dirpenn </> fp <.> "mrg"
      propfile = dirprop </> fp <.> "prop"
  (trs,props) <- propbank (pennfile,propfile,omit)
  rs <- getIdxSentProps pp (trs,props)
  liftIO $ mapM_ (showMatchedInstance <> showFeatures <> showFakeFeatures) rs
  return (trainingDataPerFile rs)  




groupFeatures :: InstanceFeature -> InstanceFeature -> InstanceFeature
groupFeatures (IFeat i roleset vo afeatss_t) (IFeat i' _ _ afeatss_f) = 
  if i /= i'
  then error "error in groupFeatues" 
  else IFeat i roleset vo (afeatss_t ++ afeatss_f)


rankArgument :: PropBankLabel -> SVMFarm -> InstanceFeature
             -> IO [(Int,RoleSet,PropBankLabel,Range,Double)]
rankArgument arglabel svmfarm ifeat = do
  let svm = if | arglabel == NumberedArgument 0 -> svmfarm^.svm_arg0
               | arglabel == NumberedArgument 1 -> svmfarm^.svm_arg1
               | otherwise                      -> error "only arg0 and arg1 are supported"
      ts = inst2vec ifeat
      ts' = filter (\x -> x^._3 == arglabel) ts
      ts_v = map (^._5.fv_nodes) ts'
  ts_result <- mapM (predict svm) ts_v
  return (zipWith (\x r -> (_5 .~ r) x) ts' ts_result)

{- 
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
-}

train :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
      -> (FilePath,FilePath)
      -> [(FilePath,IsOmit)]
      -> IO SVMFarm
train pp (dirpenn,dirprop) trainingFiles = do
  lsts <- flip mapM trainingFiles $ \(fp,omit) -> do
    r <- try $ do 
      header fp
      runEitherT $ prepareTraining pp (dirpenn,dirprop) (fp,omit)
    case r of
      Left (e :: SomeException) -> error $ "In " ++ fp ++ " exception : " ++ show e 
      Right (Right lst) -> return lst
      Right (Left e) -> error ("in run: " ++ e)
  let trainingData = mconcat lsts
  
  {- (_msg0,svm0) -}
  svm0 <- trainSVM {- (EPSILON_SVR 1 0.1) (RBF 1) [] -} (trainingData ^.training_arg0)
  {- (_msg1,svm1) -}
  svm1 <- trainSVM {- (EPSILON_SVR 1 0.1) (RBF 1) [] -} (trainingData ^.training_arg1)

  return (SVMFarm svm0 svm1)




matchRole :: SVMFarm -> SentenceInfo -> [InstanceFeature] -> EitherT String IO ()
matchRole svm sentinfo feats = do
    let sortFun = sortBy (flip compare `on` (^._5))
    resultss0 <- mapM (fmap sortFun . liftIO . rankArgument (NumberedArgument 0) svm) feats
    resultss1 <- mapM (fmap sortFun . liftIO . rankArgument (NumberedArgument 1) svm) feats
    let results = sortBy (compare `on` (^._1)) . map (\x -> head x) . filter (not.null) $ resultss0 ++ resultss1
        pt = sentinfo^.corenlp_tree
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



matchRoleForPBCorpus :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                     -> SVMFarm
                     -> ([PennTree],[Instance])
                     -> EitherT String IO ()
matchRoleForPBCorpus pp svm (trs,props) = do
  rs <- getIdxSentProps pp (trs,props)
  flip mapM_ rs $ \(_i,sentinfo,propbanktree,pr) -> do
    let ifeats = features (sentinfo,propbanktree,pr)
        ifakefeats = fakeFeatures (sentinfo,propbanktree,pr)
        feats = zipWith groupFeatures ifeats ifakefeats
    matchRole svm sentinfo feats



matchRoleForPBCorpusFile :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                         -> SVMFarm
                         -> (FilePath,FilePath)
                         -> (FilePath,IsOmit)
                         -> IO (Either String ())
matchRoleForPBCorpusFile pp svm (dirpenn,dirprop) (fp,omit) = do
  runEitherT $ do
    liftIO $ print fp
    (trs,props) <- propbank (dirpenn </> fp <.> "mrg" ,dirprop </> fp <.> "prop", omit)
    matchRoleForPBCorpus pp svm (trs,props) 
