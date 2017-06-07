{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SRL.Train where

import           AI.SVM.Simple
import           AI.SVM.Base
import           Control.Lens               hiding (levels,(<.>))
import           Control.Monad                     (void,when)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import           Data.Foldable                     (toList)
import           Data.Function                     (on)
import           Data.List                         (group,sort,sortBy,zip4)
import           Data.Maybe                        (fromJust,mapMaybe)
import           Data.Monoid                       ((<>))
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
import           FastText.Binding
import           NLP.Type.PennTreebankII
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
      let ts' = filter ((== NumberedArgument 0) . (^._3)) ts 
          ts'' = map (\x -> (1 :: Double,x^._5)) ts'
          ifakefeats = fakeFeatures (sentinfo,prs)
      fs <- concat <$> mapM (inst2vec ft) ifakefeats
      let fs' = filter ((== NumberedArgument 0) . (^._3)) fs
          fs'' = map (\x -> (-1 :: Double,x^._5)) fs'
      return (map (\(t,v) -> (t,V.map realToFrac v)) (ts''++fs''))
    return $ concat results


classifyFile ft pp svm (dirpenn,dirprop) (fp,omit) = do
  runEitherT $ do
    liftIO $ print fp
    (trs,props) <- propbank (dirpenn </> fp <.> "mrg" ,dirprop </> fp <.> "prop", omit)
    runsvm ft pp svm (trs,props) 



groupFeatures (i,roleset,voice,afeatss_t) (i',_,_,afeatss_f) = 
  if i /= i'
  then error "error in groupFeatues" 
  else (i,roleset,voice,zipWith (++) afeatss_t afeatss_f)


findArgument ft svm ifeat = do
  ts <- liftIO (inst2vec ft ifeat)
  let ts' = filter (\x -> x^._3 == NumberedArgument 0) ts
      ts_v = map (V.map realToFrac . (^._5)) ts'
      ts_result = map (predict svm) (ts_v :: [Vector Double])
      ts'' = zipWith (\x r -> (_5 .~ r) x) ts' ts_result
  return ts'' 

runsvm ft pp svm (trs,props) = do
  rs <- getIdxSentProps pp (trs,props)
  flip mapM_ rs $ \(i,sentinfo,pr) -> do
    let ifeats = features (sentinfo,pr)
        ifakefeats = fakeFeatures (sentinfo,pr)
        sortFun = sortBy (flip compare `on` (^._5))
        
    resultss <- mapM (fmap sortFun . findArgument ft svm) (zipWith groupFeatures ifeats ifakefeats)
    let pt = sentinfo^.corenlp_tree
        ipt = mkPennTreeIdx pt
        terms = map (^._2) . toList $ pt
    liftIO $ putStrLn "======================================================================================="        
    liftIO $ TIO.putStrLn (T.intercalate " " terms)
    liftIO $ putStrLn "======================================================================================="
    liftIO $ flip mapM_ resultss $ \results -> do
      when ((not.null) results) $ do
        let result = head results
        let mmatched = matchR (result^._4) ipt
        case mmatched of
          Nothing -> TIO.putStrLn "no matched?"
          Just matched -> let txt = T.intercalate " " (map (^._2._2) (toList matched))
                          in putStrLn $ formatResult result txt 

formatResult (n,(lemma,sensenum),label,range,value) txt =
  printf "%d %15s.%2s %8s %8s %f %s" n lemma sensenum (pbLabelText label) (show range) value txt
    
