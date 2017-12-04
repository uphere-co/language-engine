{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Applicative         ((<$>),(<*>))
import           Control.Lens                ((^.),(^..),ix,to,makeLenses,_2)
import           Control.Monad               (void)
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.List                   (zip4,zip5)
import           Data.Maybe                  (catMaybes,mapMaybe)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Data.Text.Lazy.IO    as TLIO
import           Options.Applicative
import           System.Directory            (getCurrentDirectory,setCurrentDirectory)
import           System.FilePath             ((<.>))
import           System.Process              (readProcess)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--
import           Lexicon.Data                (loadLexDataConfig)
import           MWE.Util                    (mkTextFromToken)
import           NLP.Type.CoreNLP            (sentenceLemma,sentenceToken)
import           NLP.Type.PennTreebankII     (Lemma(..))
import           Text.Format.Dot             (mkLabelText)
--
import           SRL.Analyze                 (loadJVM,loadConfig,printMeaningGraph)
import           SRL.Analyze.Format          (dotMeaningGraph,formatDocStructure)
import           SRL.Analyze.Match.MeaningGraph (meaningGraph,tagMG)
import           SRL.Analyze.SentenceStructure (sentStructure,mkWikiList)
import           SRL.Analyze.Type
import           SRL.Analyze.UKB             (runUKB)
--
import Type


data TestConfig = TestConfig { _tconfig_lexconfig :: FilePath
                             , _tconfig_testset :: FilePath
                             , _tconfig_outputdir     :: FilePath
                             }
                deriving Show

makeLenses ''TestConfig

pOptions :: Parser TestConfig
pOptions = TestConfig
           <$> strOption (long "config" <> short 'c' <> help "config file")
           <*> strOption (long "file" <> short 'f' <> help "test text json file")
           <*> strOption (long "outdir" <> short 'o' <> help "output directory") 



progOption :: ParserInfo TestConfig
progOption = info pOptions (fullDesc <> progDesc "test generation")


createDotPng :: FilePath -> [(Int,Text,MeaningGraph)] -> IO (FilePath,[(Int,Text,MeaningGraph)])
createDotPng fn imglst = do
  flip mapM imglst $ \(i,title,mg) -> do
    let fullfilename = fn ++ "_" ++ show i
    let dotstr = dotMeaningGraph Nothing {- (mkLabelText title) -} mg
    TIO.putStrLn dotstr
    TIO.writeFile (fullfilename <.> "dot") dotstr
    void (readProcess "dot" ["-Tpng", fullfilename <.> "dot","-o" ++ fullfilename <.> "png"] "")
  return (fn,imglst)


process apredata t = do
  TIO.putStrLn (t^.test_id)
  let dainput@(DocAnalysisInput sents sentidxs sentitems _ mptrs _ mtmxs) = t^.test_dainput
      lmass = sents ^.. traverse . sentenceLemma . to (map Lemma)
      mtokenss = sents ^.. traverse . sentenceToken  
      mergedtags = t^.test_ner
  synsetss <- runUKB (apredata^.analyze_wordnet)(sents,mptrs)
  let sstrs = mapMaybe (sentStructure apredata mergedtags) (zip5 ([1..] :: [Int]) sentidxs lmass mptrs synsetss)
      -- dstr = DocStructure mtokenss sentitems mergedtags sstrs
      -- sstrs1 = catMaybes sstrs
      mgs = map (meaningGraph apredata) sstrs
      imgs = flip map (zip4 [1..] mgs sstrs mtokenss) $ \(i,mg,sstr,mtoks) ->
               let title = mkTextFromToken mtoks
                   wikilst = mkWikiList sstr
                   mg' = tagMG mg wikilst
               in (i,title,mg')
  createDotPng (T.unpack (t^.test_id)) imgs



mainHtml fileimgs =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Test"
    H.body $ do
      H.ul $ do
        mapM_ onefigure fileimgs


onefigure (fn,imglst) =
  H.li $ do
    (H.div (H.toHtml (T.pack fn)))
    (H.div (H.toHtml (imglst^.ix 0._2)))
    H.img ! A.src (H.stringValue (fn ++ "_1.png"))

createIndex :: [(FilePath,[(Int,Text,MeaningGraph)])] -> IO ()
createIndex fileimgs = TLIO.writeFile "index.html" (renderHtml (mainHtml fileimgs))




main = do
  tcfg <- execParser progOption
  cfg <- loadLexDataConfig (tcfg^.tconfig_lexconfig) >>= \case Left err -> error err
                                                               Right x -> return x
  (apredata,_netagger) <- loadConfig True cfg
  
  putStrLn "create performance testing set"
  bstr <- BL.readFile (tcfg^.tconfig_testset)
  case eitherDecode' bstr :: Either String [Test] of
    Left err -> putStrLn err
    Right lst -> do
      cwd <- getCurrentDirectory
      setCurrentDirectory (tcfg^.tconfig_outputdir)
      fileimgs <- mapM (process apredata) lst
      createIndex fileimgs
      setCurrentDirectory cwd
