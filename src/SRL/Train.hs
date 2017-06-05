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
        -> IO ()
process ft pp (dirpenn,dirprop) (fp,omit) = do
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
    liftIO $ flip mapM_ rs $ \(_,sentinfo,prs) -> do
      let ifeats = features (sentinfo,prs)
      case ifeats of
        [] -> return ()
        (ifeat:_) -> do
          -- word2vec ft (ifeat ^. _2 . _1) >>= print
          
          let xs = concat (ifeat ^. _4)
          flip mapM_ xs $ \x -> do
            -- print $ (x^._1,pblabel2vec (x^._1))) xs
            -- print (x^._2._2)
            -- print (ptp2vec (x^._2._2))
            argnode2vec ft x >>= print
