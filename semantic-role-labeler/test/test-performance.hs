{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Control.Applicative         ((<$>),(<*>))
import           Control.Lens                ((^.),(^..),ix,to,makeLenses,_2)
import           Control.Monad               (void)
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.IntMap                 (IntMap)
import           Data.List                   (zip4,zip5)
import           Data.Maybe                  (mapMaybe)
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
-- import           Lexicon.Data                (loadLexDataConfig)
import           MWE.Util                    (mkTextFromToken)
import           NER.Type                    (CompanyInfo)
import           NLP.Type.CoreNLP            (sentenceLemma,sentenceToken)
import           NLP.Type.PennTreebankII     (Lemma(..))
--
import           SRL.Analyze                 (loadConfig)
import           SRL.Analyze.Config          (SRLCOnfig)
import           SRL.Analyze.Format          (dotMeaningGraph)
import           SRL.Analyze.Match.MeaningGraph (meaningGraph,tagMG)
import           SRL.Analyze.SentenceStructure (sentStructure,mkWikiList)
import           SRL.Analyze.Type
import           SRL.Analyze.UKB             (runUKB)
--
import Type


data TestConfig = TestConfig { _tconfig_srlconfig :: FilePath
                             , _tconfig_testset   :: FilePath
                             , _tconfig_outputdir :: FilePath
                             , _tconfig_old       :: FilePath
                             , _tconfig_new       :: FilePath
                             }
                deriving Show

makeLenses ''TestConfig

pOptions :: Parser TestConfig
pOptions = TestConfig
           <$> strOption (long "config" <> short 'c' <> help "config file")
           <*> strOption (long "file" <> short 'f' <> help "test text json file")
           <*> strOption (long "outdir" <> short 'd' <> help "output directory")
           <*> strOption (long "old" <> short 'o' <> help "old version")
           <*> strOption (long "new" <> short 'n' <> help "new version")



progOption :: ParserInfo TestConfig
progOption = info pOptions (fullDesc <> progDesc "test generation")


createDotPng :: String -> FilePath -> [(Int,Text,MeaningGraph)] -> IO (FilePath,[(Int,Text,MeaningGraph)])
createDotPng new fn imglst = do
  flip mapM imglst $ \(i,_title,mg) -> do
    let fullfilename = new ++ "_" ++ fn ++ "_" ++ show i
    let dotstr = dotMeaningGraph Nothing mg
    TIO.putStrLn dotstr
    TIO.writeFile (fullfilename <.> "dot") dotstr
    void (readProcess "dot" ["-Tpng", fullfilename <.> "dot","-o" ++ fullfilename <.> "png"] "")
  return (fn,imglst)


process :: AnalyzePredata -> IntMap CompanyInfo -> String -> Test -> IO (FilePath, [(Int,Text,MeaningGraph)])
process apredata companyMap new t = do
  TIO.putStrLn (t^.test_id)
  let DocAnalysisInput sents sentidxs _ _ mptrs _ _ = t^.test_dainput
      lmass = sents ^.. traverse . sentenceLemma . to (map Lemma)
      mtokenss = sents ^.. traverse . sentenceToken
      mergedtags = t^.test_ner
  synsetss <- runUKB (apredata^.analyze_wordnet)(sents,mptrs)
  let sstrs = mapMaybe (sentStructure apredata mergedtags) (zip5 ([1..] :: [Int]) sentidxs lmass mptrs synsetss)
      mgs = map (meaningGraph apredata) sstrs
      imgs = flip map (zip4 [1..] mgs sstrs mtokenss) $ \(i,mg,sstr,mtoks) ->
               let title = mkTextFromToken mtoks
                   wikilst = mkWikiList companyMap sstr
                   mg' = tagMG mg wikilst
               in (i,title,mg')
  createDotPng new (T.unpack (t^.test_id)) imgs


mainHtml :: (String,String) -> [(FilePath,[(Int,Text,MeaningGraph)])] -> H.Html
mainHtml (old,new) fileimgs =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Test"
    H.body $ do
      H.ul $ do
        mapM_ (onefigure (old,new)) fileimgs


onefigure :: (String,String) -> (FilePath,[(Int,Text,MeaningGraph)]) -> H.Html
onefigure (old,new) (fn,imglst) =
  H.li $ do
    (H.div (H.h1 (H.toHtml (T.pack fn))))
    (H.div (H.h3 (H.toHtml (imglst^.ix 0._2))))
    H.div $ do
      H.div ! A.style "display: inline-block;" $ do
        H.img ! A.src (H.stringValue (old ++ "_" ++ fn ++ "_1.png")) ! A.width "800" ! A.style "border: solid 1px; margin: 10px 10px 10px 10px;"
        H.div (H.h1 (H.toHtml old))
      H.div  ! A.style "display: inline-block;" $ do
        H.img ! A.src (H.stringValue (new ++ "_" ++ fn ++ "_1.png")) ! A.width "800" ! A.style "border: solid 1px; margin: 10px 10px 10px 10px;"
        H.div (H.h1 (H.toHtml new))
    H.hr


createIndex :: (String,String) -> [(FilePath,[(Int,Text,MeaningGraph)])] -> IO ()
createIndex (old,new) fileimgs = TLIO.writeFile "index.html" (renderHtml (mainHtml (old,new) fileimgs))


main :: IO ()
main = do
  tcfg <- execParser progOption
  cfg <- do e <- eitherDecode' @SRLConfig <$> readFile (tcfg^.tconfig_srlconfig))
            case e of
              Left err -> error err
              Right x -> return x
  (apredata,_netagger,_,companyList) <- loadConfig (True,True) cfg
  let old = tcfg^.tconfig_old
      new = tcfg^.tconfig_new
  putStrLn "create performance testing set"
  bstr <- BL.readFile (tcfg^.tconfig_testset)
  case eitherDecode' bstr :: Either String [Test] of
    Left err -> putStrLn err
    Right lst -> do
      cwd <- getCurrentDirectory
      setCurrentDirectory (tcfg^.tconfig_outputdir)
      fileimgs <- mapM (process apredata companyList new) lst
      createIndex (old,new) fileimgs
      setCurrentDirectory cwd
