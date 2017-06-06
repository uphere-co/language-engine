{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SRL.Train where

import           AI.SVM.Simple
import           AI.SVM.Base
import           Control.Lens               hiding (levels,(<.>))
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import           Data.List                         (group,sort,zip4)
import           Data.Maybe                        (fromJust,mapMaybe)
import           Data.Monoid                       ((<>))
import qualified Data.Sequence              as Seq
import           Data.Vector.Storable              (Vector)
import qualified Data.Vector.Storable       as V
import           Foreign.C.Types
import           Language.Java              as J
import           System.FilePath                   ((</>),(<.>))
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           FastText.Binding
import           PropBank.Type.Prop
import           PropBank.Util
--
import           SRL.Feature 
import           SRL.PropBankMatch
import           SRL.Vectorize


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
        rs = map (\(i,((pt,tr,dep,sent),pr)) -> (i,SentInfo sent pt tr dep,pr))
           . merge (^.inst_tree_id) (zip4 pts trs deps sents)
           $ props
    return rs

process :: FastText
        -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
        -> (FilePath,FilePath)
        -> (FilePath,IsOmit)
        -> IO (Either String ([(Double,Vector Double)]))
process ft pp (dirpenn,dirprop) (fp,omit) = do
  let pennfile = dirpenn </> fp <.> "mrg"
      propfile = dirprop </> fp <.> "prop"
  runEitherT $ do
    (trs,props) <- propbank (pennfile,propfile,omit)
    rs <- getIdxSentProps pp (trs,props)
    liftIO $ mapM_ (showMatchedInstance <> showFeatures <> showFakeFeatures) rs
    results :: [[(Double,Vector Double)]]
     <-  liftIO $ flip mapM rs $ \(_,sentinfo,prs) -> do
      let ifeats = features (sentinfo,prs)
      ts <- concat <$> mapM (inst2vec ft) ifeats
      let ts' = filter ((== NumberedArgument 0) . (^._2)) ts 
          ts'' = map (\x -> (1 :: Double,x^._4)) ts'
          ifakefeats = fakeFeatures (sentinfo,prs)
      fs <- concat <$> mapM (inst2vec ft) ifakefeats
      let fs' = filter ((== NumberedArgument 0) . (^._2)) fs
          fs'' = map (\x -> (-1 :: Double,x^._4)) fs'
      return (map (\(t,v) -> (t,V.map realToFrac v)) (ts''++fs''))
    return $ concat results


findArgument ft pp svm (dirpenn,dirprop) (fp,omit) = do
  runEitherT $ do
    liftIO $ print fp
    (trs,props) <- propbank (dirpenn </> fp <.> "mrg" ,dirprop </> fp <.> "prop", omit)
    runsvm ft pp svm (trs,props) 


runsvm ft pp svm (trs,props) = do
  rs <- getIdxSentProps pp (trs,props)
  flip mapM_ rs $ \(i,sentinfo,pr) -> do
    let ifeats = features (sentinfo,pr)
        ifakefeats = fakeFeatures (sentinfo,pr)

    ts <- liftIO (concat <$> mapM (inst2vec ft) ifeats)
    fs <- liftIO (concat <$> mapM (inst2vec ft) ifakefeats)

    

    let ts' = filter (\x -> x^._2 == NumberedArgument 0) ts
        fs' = filter (\x -> x^._2 == NumberedArgument 0) fs

    let ts_v = map (V.map realToFrac . (^._4)) ts'
    let fs_v = map (V.map realToFrac . (^._4)) fs'

    
    let ts_result = map (predict svm) (ts_v :: [Vector Double])
        fs_result = map (predict svm) (fs_v :: [Vector Double])


    let ts'' = zipWith (\x r -> (_4 .~ r) x) ts' ts_result
        fs'' = zipWith (\x r -> (_4 .~ r) x) fs' fs_result

    liftIO $ putStrLn "========================================="
    liftIO $ mapM_ print ts''
    liftIO $ putStrLn "-----------------------------------------"
    liftIO $ mapM_ print fs''

  {- 
  let  -- pr = props !! 2
      tr = trs !! n
      prs = filter (\x -> x^.inst_tree_id == n) props -}

  -- return $ map (V.map realToFrac) (concat results)
  {- liftIO $ do
    print ts'
    print fs' -}

