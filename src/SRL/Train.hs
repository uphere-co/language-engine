{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SRL.Train where

import           Control.Lens               hiding (levels,(<.>))
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import           Data.List                         (zip4)
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

process :: FastText
        -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
        -> (FilePath,FilePath)
        -> (FilePath,IsOmit)
        -> IO (Either String ([(CFloat,Vector CFloat)]))
process ft pp (dirpenn,dirprop) (fp,omit) = do
  let pennfile = dirpenn </> fp <.> "mrg"
      propfile = dirprop </> fp <.> "prop"
  runEitherT $ do
    (trs,props) <- propbank (pennfile,propfile,omit)
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
    liftIO $ mapM_ (showMatchedInstance <> showFeatures <> showFakeFeatures) rs
    results :: [[(CFloat,Vector CFloat)]]
     <-  liftIO $ flip mapM rs $ \(_,sentinfo,prs) -> do
      let ifeats = features (sentinfo,prs)
      ts <- concat <$> mapM (inst2vec ft) ifeats
      let ts' = filter ((== NumberedArgument 0) . (^._1)) ts 
          ts'' = map (\(_,v) -> (1,v)) ts'
          ifakefeats = fakeFeatures (sentinfo,prs)
      fs <- concat <$> mapM (inst2vec ft) ifakefeats
      let fs' = filter ((== NumberedArgument 0) . (^._1)) fs
          fs'' = map (\(_,v) -> (-1,v)) fs'
      return (ts''++fs'')
    return (concat results)


test ft pp svm = do
  runEitherT $ do
    let dirpenn = "/scratch/wavewave/MASC/Propbank/Penn_Treebank-orig/data/written"
        dirprop = "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written"
    (trs,props) <- propbank (dirpenn </> "wsj_0189" <.> "mrg" ,dirprop </> "wsj_0189" <.> "prop", Omit)
    let tr = head trs
        pr = head props
        doc = mkDocFromPennTree tr
    rdoc <- liftIO $ do
      ann <- annotate pp doc
      protobufDoc ann
    d <- hoistEither rdoc
    let sent = Seq.index (d^.D.sentence) 0
    dep <- hoistEither (sentToDep sent)
    let cpt = fromJust (sent ^.S.parseTree)
        pt = decodeToPennTree cpt
        r = (SentInfo sent pt tr dep,pr)

        ifeats = features r
        ifakefeats = fakeFeatures r
    ts <- concat <$> mapM (inst2vec ft) ifeats
    fs <- concat <$> mapM (inst2vec ft) ifakefeats
      
    liftIO $ print (head ts)

    liftIO $ print (head fs)
