{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Applicative                   ((<$>),(<*>))
import           Control.Lens                          ((^.),(^?),(.~),(^..),(&),ix,to,_2,_Just,makeLenses)
import           Data.Aeson                            (encode)
-- import           Data.Aeson.Encode.Pretty              (encodePretty)
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy.Char8       as BL
import           Data.Default                          (def)
import qualified Data.IntMap                      as IM
import           Data.Maybe                            (fromJust, mapMaybe)
import           Data.Monoid                           ((<>))
import           Data.Text                             (Text)
import qualified Data.Text                        as T
import           Data.Time.Calendar                    (fromGregorian)
import           Language.Java                    as J
import qualified Options.Applicative              as O
import           System.Environment                    (getEnv)
import           System.IO                             (withFile,IOMode(..))
import           Text.ProtocolBuffers.WireMessage      (messageGet)
import           Text.Printf                           (printf)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import qualified CoreNLP.Proto.CoreNLPProtos.Timex           as Tmx
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex       as Tmx
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as Tmx
import           CoreNLP.Simple                (annotate,prepare,protobufDoc,serializeTimex)
import           CoreNLP.Simple.Convert        (convertSentence,cutf8,decodeToPennTree,mkLemmaMapFromPSent,sentToDep,sentToNER)
import           CoreNLP.Simple.Type           (Document(Document),constituency,depparse,lemma,ner,postagger,sutime,tokenizer
                                               ,words2sentences)
import           CoreNLP.Simple.Util           (getTKTokens)
import           Lexicon.Data                  (loadLexDataConfig)
import           NLP.Type.PennTreebankII       (Lemma(..))
import           NLP.Type.TagPos               (TagPos(..), TokIdx)
--
import           SRL.Analyze                   (loadJVM,loadConfig)
import           SRL.Analyze.CoreNLP           (runParser)
import           SRL.Analyze.SentenceStructure (nerDocument)
--
import Type

testsets :: [(Text,Text)]
testsets =
  [ ("plan_to_seek", "Chilean copper mine Collahuasi, a joint venture of Anglo American and Glencore, is postponing its plan to seek approval of an environmental impact study for an expansion until next year, the company told Reuters on Tuesday.")
  , ("rise_6_6_percent", "General Motors Co's vehicle sales in China rose 6.6 percent in September from a year earlier to 366,305 vehicles, following a 12 percent increase in August and a 6.3 percent increase in July, the Detroit automaker said on Wednesday.")
  , ("invest_210_million", "Dow Chemical said in a statement on Tuesday it would invest $210 million to improve a petrochemical complex in Argentina's Buenos Aires province over the next two years.")
  , ("sansiri", "Sansiri Pcl said on Wednesday it would invest $80 million in overseas markets, including a 35 percent stake purchase in a U.S.-based hotel chain, as the Thai real estate developer seeks to expand beyond its core business.")
  , ("roger_as_director", "Britain's Metro Bank Plc said on Wednesday it appointed Roger Fenwick as director of specialist sectors for its commercial business.")
  , ("fda_approve", "The U.S. Food and Drug Administration on Wednesday approved Abbott Laboratories' glucose monitoring device for adults with diabetes, allowing millions of people to track their blood sugar levels without having to prick their fingers.")
  , ("israelli_chip_maker", "Israeli chip maker Mellanox Technologies saw its shares surge on the Nasdaq on Tuesday after activist hedge fund Starboard Value LP bought a 10.7 percent stake to influence strategy.")
  , ("james_dyson", "James Dyson, the billionaire inventor of the bagless vacuum cleaner, said his company was building a \"radical\" all-electric car for launch in 2020.")
  , ("nfl_criticism", "NFL team owners will consider requiring players to stand for the U.S. national anthem after President Donald Trump on Tuesday stepped up his criticism of silent player protests against racial injustice by targeting the league on taxes.")
  , ("sf_motors", "SF Motors Inc, a California-based electric vehicle (EV) unit of China's Chongqing Sokon Industry Group Co Ltd, on Thursday said it has bought an EV and battery tech firm headed by former Tesla Inc executive Martin Eberhard for $33 million.")
  , ("domino_pizza", "Domino's Pizza Group Plc said its German joint venture, in which it owns a third of the stake, would buy Germany's largest independent pizza chain, Hallo Pizza, to expand its business in the country.")
  , ("monsanto_weedkiller", "Monsanto Co sued Arkansas agricultural officials on Friday to stop proposed restrictions on the use of a weed killer linked to widespread U.S. crop damage, setting up a legal battle between the agrichemical company and a major farm state.")
  , ("hyperloop_virgin", "Los-Angeles-based company, Hyperloop One said billionaire Richard Brandson's Virgin Group has invested in the company to form a strategic partnership.")
  , ("brazil_wto", "A Brazilian business leader urged a World Trade Organization forum on Wednesday to use a technology initiative to help smaller firms gain better access to global commerce and trade.")
  , ("preposed_adjunctcp", "After he wrote the book, the police planned to investigate him.")
  , ("brazil_steel", "Brazilian steelmaker Compania Siderugica Nacional SA plans to sell bonds on international markets in an effort to improve its debt profile, Benjamin Steinbruch, chief executive officer, said on Friday.")
  , ("toyota_selfdriving", "Toyota Motor Corp on Monday said it would begin testing self-driving cars.")
  , ("free_relative_who", "I don't know who write the book.")
  , ("free_relative_what", "I don't know what he wrote.") ]

listTimexToTagPos :: Tmx.ListTimex -> [TagPos TokIdx (Maybe Text)]
listTimexToTagPos tmxs = tmxs^.. Tmx.timexes . traverse . to convert
  where
    fi = fromIntegral
    convert t = TagPos (fi (t^.Tmx.tokenBegin), fi (t^.Tmx.tokenEnd), t^?Tmx.timex . Tmx.value . _Just . to cutf8)



data TestConfig = TestConfig { _tconfig_lexconfig :: FilePath
                             , _tconfig_testset   :: FilePath
                             }
                deriving Show

makeLenses ''TestConfig

pOptions :: O.Parser TestConfig
pOptions = TestConfig
           <$> O.strOption (O.long "config" <> O.short 'c' <> O.help "config file")
           <*> O.strOption (O.long "file" <> O.short 'f' <> O.help "test text json file")


progOption :: O.ParserInfo TestConfig
progOption = O.info pOptions (O.fullDesc <> O.progDesc "test generation")

main = do
  putStrLn "generate test text"
  -- let txt = (testsets^.ix 0._2)
  tcfg <- O.execParser progOption
  clspath <- getEnv "CLASSPATH"
  cfg <- loadLexDataConfig (tcfg^.tconfig_lexconfig)  >>= \case Left err -> error err
                                                                Right x -> return x
  (apredata,netagger,_,_) <- loadConfig False cfg
  withFile (tcfg^.tconfig_testset) WriteMode $ \h ->
    J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
      pp <- loadJVM
      lst <- flip mapM testsets $ \(tid,txt) -> do
               dainput <- runParser pp txt
               let ner = nerDocument apredata netagger dainput
               return (Test tid txt dainput ner)
      BL.hPutStrLn h (encode lst)
