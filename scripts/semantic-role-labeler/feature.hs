{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens               hiding (levels,(<.>))
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Char8      as B
import           Data.Default
import           Data.List                         (zip4)
import           Data.Monoid                       ((<>))
import           Data.Maybe                        (mapMaybe)
import qualified Data.Sequence              as Seq
import           Language.Java              as J
import           Options.Applicative
import           System.Environment                (getEnv)
import           System.FilePath                   ((</>),(<.>))
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           PropBank.Type.Prop
import           PropBank.Util
--
import           SRL.Old.Feature
import           SRL.Old.PropBankMatch
import           SRL.Old.Type

data ProgOption = ProgOption { penndir :: FilePath
                             , propdir :: FilePath
                             , filename :: FilePath
                             , omitflag :: Bool
                             } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "penn" <> short 'n' <> help "Penn Treebank directory")
                      <*> strOption (long "prop" <> short 'p' <> help "PropBank directory")
                      <*> strOption (long "name" <> short 'f' <> help "File name")
                      <*> switch (long "omit" <> short 'o' <> help "Omit lemma-type or not")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "Test features for PropBank corpus")

  
main :: IO ()
main = do
  opt <- execParser progOption
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do 
    void . runEitherT $ do
      let omit = case omitflag opt of 
                   True -> Omit
                   False -> NoOmit
      (trs,props) <- propbank (penndir opt </> filename opt <.> "mrg", propdir opt </> filename opt <.> "prop",omit)
      rdocs <- liftIO $ do
        let pcfg = def & ( tokenizer .~ True )
                       . ( words2sentences .~ True )
                       . ( postagger .~ True )
                       . ( lemma .~ True )
                       . ( sutime .~ False )
                       . ( depparse .~ True )
                       . ( constituency .~ True )
                       . ( ner .~ False )
        pp <- prepare pcfg
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
      liftIO $ mapM_ (showMatchedInstance <> showFeatures <> showFakeFeatures) rs

