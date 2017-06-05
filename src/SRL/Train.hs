{-# LANGUAGE DataKinds #-}

module SRL.Train where

import           Control.Lens               hiding (levels,(<.>))
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import           Data.List                         (zip4)
import           Data.Maybe                        (mapMaybe)
import           Data.Monoid                       ((<>))
import qualified Data.Sequence              as Seq
import           Language.Java              as J
import           System.FilePath                   ((</>),(<.>))
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           PropBank.Type.Prop
import           PropBank.Util
--
import           SRL.Feature 
import           SRL.PropBankMatch

process :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
        -> (FilePath,FilePath)
        -> (FilePath,IsOmit)
        -> IO ()
process pp (dirpenn,dirprop) (fp,omit) = do
  let pennfile = dirpenn </> fp <.> "mrg"
      propfile = dirprop </> fp <.> "prop"
  void . runEitherT $ do
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

