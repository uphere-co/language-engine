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
  , ("free_relative_what", "I don't know what he wrote.")
  , ("step_down", "Hewlett Packard Enterprise Co said on Tuesday that Meg Whitman was stepping down as chief executive officer.")
  , ("its_plan_to_seek", "Chilean copper mine Collahuasi, a joint venture of Anglo American and Glencore, is postponing its plan to seek approval of an environmental impact study for an expansion until next year, the company told Reuters on Tuesday.")
  , ("as_role","HSBC said it appointed Philip Kunz as head of global private banking for South East Asia.")
  , ("used_to","A former top HSBC Holdings Plc executive threw his Scottish client \"under the bus\" by using his knowledge of its large planned currency transaction to do his own trading first, a U.S. prosecutor said on Monday at the start of a closedly-watched trail.")
  , ("russia_ordered","Russia ordered the U.S. to cut its diplomatic staff in Russia by 755 people.")
  , ("theresa_may","U.K. Prime Minister Theresa May said she was reducing the country's terrorism-threat level from critical to severe, less than a week after a suicide bomber killed 22 people in a terror attack in Manchester.")
  , ("infosys","India's Infosys Ltd. said it plans to hire 10,000 American employees in the next two years, following criticism from the Trump administration that the company and other outsourcing firms are unfairly taking jobs away from U.S. workers.")
  , ("hurricane_irma","Officials were struggling Monday to reach the majority of the Florida Keys amid early indications that Huricane Irma showed the island chain little mercy.")
  , ("bmo_global","BMO Global Asset Management, owned by Canada's Bank of Montreal, said on Monday it has appointed Fadi Khoury as head of Middle East and North Africa.")
  , ("thursday","Roku's first day of trading Thursday saw shares rise nearly 70 percent.")
  , ("topic_volkswagen","Volkswagen's fixes for diesel vehicles in Europe are running smoothly, a spokesman said after the company said its fixes in North America may cost 2.5 billion euros more than expected.")
  , ("uber_shut_down","Uber Technologies Inc. on Wednesday confirmed it is shutting down its U.S. auto-leasing business, months after it discovered it was losing 18-times more money per vehicle than previously thought.")
  , ("microsoft_siri","Microsoft Corp Chief Executive Officer Satya Nadella said on Wednesday the company's search engine, Bing, will focus on expanding in the PC search market after losing its deal with Apple Inc's Siri.")
  , ("alzheimer","Axovant Sciences Ltd said on Tuesday its experimental drug targeting Alzheimer's patients failed to meet the main goals in a late-stage trial.")
  , ("alibaba_raise","Chinese e-commerce giant Alibaba Group Holding Ltd is raising its stake in logistics affiliate Cainiao Smart Logistics Network Ltd to 51 percent from 47 percent by investing 5.3 billion yuan.")
  , ("aig_risk","The council voted six to three that AIG was no longer a risk to financial stability.")
  , ("sec_maksim","The SEC said it charged Maksim Zaslavskiy along with REcoin Group Foundation and DRC World with selling unregistered cryptocurrency securities reputedly backed by investments in real estate and diamonds, when it found no such investment had taken place.")
  , ("citigroup","Citigroup Inc and Lehman Brothers Holdings Inc resolved a fight over $2.1 billion that dates to the financial crisis era after Citigroup agreed to give back $1.74 billion to the estate of the investment bank, according to Bloomberg.")
  , ("resign_today","Sanders said in a statement that Price \"offered his resignation earlier today and the president accepted.\"")
  , ("why_consumers","Senators pressed Equifax CEO Wednesday on why consumers shouln't have power over the data that companies collect on them.")
  , ("basf_friday","BASF said on Friday it had agreed to buy significant parts of Bayer's seed and non-selective herbicide businesses for 5.9 billion euros ($7 billion) in cash.")
  , ("kenyan_stocks","Kenyan stocks and bonds fell on Wednesday after opposition leader Raila Odinga pulled out of a repeat presidential election set for Oct. 26.")
  , ("defense_aero","Defense and aerospace company Lockheed Martin Corp. said Thursday its board of directors has authorized buying an additional $2 billion of the company's stock under its share buyback program.")
  , ("james_bullard","St. Louis Fed President James Bullard told Business Insider that there was \"no hurry to raise rates\" with inflation below target.")
  , ("toyota_test","Toyota said it would begin testing self-driving electric cars around 2020, which will use artificial intelligence to engage with drivers.")
  , ("motorized_revenue","Motorized revenue fell 4.4% to $226.2 million, while towable revenue increased to $228.7 million from $26.6 million, boosted by the addition of $193.4 million in revenue from the Grand Design acquisition.")
  , ("adults_lose","Adults tend to lose this enzyme unless they continue taking cow's milk.")
  , ("toyota_nrc","Toyota Motor Corp would begin testing self-driving cars, which needs technology advances, around 2020.")
  , ("uk_boutique","UK-based boutique capital markets firm Demica said it appointed Maurice Benisty as chief commercial officer.")
  , ("ford_motor","Ford Motor Co said on Wednesday it would recall about 1.3 million vehicles in North America to add a water shield to side door latches.")
  , ("commodity_futures","The Commodity Futures Trading Commission said on Thursday it had ordered Morgan Stanley to pay a $350,000 penalty for failing to comply with rules that require large traders to include large amounts of futures and options data in reports to the agency.")
  , ("volkswagen","Volkswagen AG confirmed Tuesday it has agreed to pay $69 million to settle New Jersey state diesel emissions claims, one of the last major outstanding diesel legal issues the German automaker faces in the United States.")
  , ("us_refiners","U.S. refiners are set to blow past quarterly earnings expectations after margins surged to a two-year peak on the back of a crippling hurricane season that squeezed already tight gasoline and diesel supplies.")
  , ("nestle","Nestle, the world's largest packaged foods company, said it aims to use only eggs from hens that live outside of cages, becoming the latest foodmaker to react to concerns about animal welfare.")
  , ("arco_corp","U.S.-based AGCO Corp, an agricultural equipment maker, is still evaluating the acquisition of Brazilian silo maker Kepler Weber SA, around nine months after revealing plans to launch a tender offer for all the outstanding shares, a senior executive said on Friday.")
  , ("french_retailer","French retailer Auchan [AUCH.UL] said on Monday it had not been approached by E-commerce giant Amazon  about deals or partnerships in Europe, with speculation still rife that Amazon may be eyeing European transactions.")
  , ("israeli","Israeli chip maker Mellanox Technologies saw its shares surge on the Nasdaq on Tuesday after activist hedge fund Starboard Value LP bought a 10.7 percent stake to influence strategy.")
  , ("weed_killer","A weed killer blamed for damaging millions of acres of U.S. crops this summer did not reduce yields for most of the soybeans checked by BASF SE , which makes a version of the herbicide, the company said on Friday.")
  , ("tesla_inc","Tesla Inc has completed construction of the world's largest lithium ion battery in Australia, putting it on track to meet a 100-day deadline for switching the battery packs on, the South Australian government said on Thursday.")
  , ("bayer_antitrust","Bayer said it expected antitrust authorities to make the planned acquisition of Monsanto conditional on more asset sales after agreeing to sell seed and herbicide businesses for 5.9 billion euro ($7 billion) to BASF.")
  , ("peter_thiel","Venture capitalist Peter Thiel may be looking to buy online news site Gawker.com, BuzzFeed said on Wednesday.")
  , ("walt_disney","Walt Disney Co animation executive John Lasseter told company staff on Tuesday he was taking a six-month leave of absence following what he called \"missteps\" including unwanted hugs that made employees uncomfortable, according to a memo seen by Reuters.")
  , ("eli_lilly","Eli Lilly and Co said on Tuesday its drug to treat non-small cell lung cancer failed to meet the main goal of improving overall survival in patients in a late-stage study.")
  , ("un_proposal","The United Nations said on Tuesday it had accepted a proposal from Iraq to pay 0.5 percent of its 2018 oil proceeds towards compensation for $4.6 billion owed to Kuwait for destruction of its oil facilities during the 1990-91 Gulf War occupation.")
  , ("russia_watchdog","Russia's communication watchdog Roskomnadzor said on Tuesday it would consider retaliatory measures against Alphabet Inc's Google over any action it takes in respect of Russian media outlets RT and Sputnik, Interfax news agency reported.")
  , ("apple_skype","Apple Inc  said on Tuesday it has removed several apps including Skype, Microsoft Corp's internet phone call and messaging service, from its app store in China after the country's government pointed to violations of local laws.")
  , ("bank_sharjah","Bank of Sharjah and Invest Bank are in merger talks that could create an institution with about 50.6 billion dirhams ($13.8 billion) of assets, sources familiar with the matter told Reuters.")
  , ("toyota_nrc2","Toyota Motor Corp on Monday said it would begin testing self-driving electric cars around 2020, which will use artificial intelligence (AI) to engage with drivers, as the company competes with tech firms to develop new vehicles.")
  , ("metro_acquisition","Canada's Metro Inc said on Wednesday it would sell a major portion of its stake in Alimentation Couche Tard Inc to fund its C$4.5 billion acquisition of pharmacy chain Jean Coutu Group.")
  , ("petrobras","Petrobras Chief Executive Officer Pedro Parente said he will meet this month with the CEO of China National Petroleum Corp in Brazil to discuss the details of their partnership to build a refinery complex in Rio de Janeiro.")
  , ("fcc_vote","The U.S. Federal Communications Commission voted 3-2 on Thursday to allow broadcasters to voluntarily use a new technology to improve picture quality and allow better reception on mobile phones and give advertisers dramatically more data about viewing habits.")
  , ("jbs_pennsylvania","Unspecified pests have prompted JBS USA to halt production temporarily at a Pennsylvania beef processing plant that accounts for about 2 percent of U.S. daily cattle slaughter.")
  , ("russian_judge","A Russian judge summoned the boss of the country's biggest oil company to testify next week in the trial of a former economy minister accused of taking a bribe from him, after Rosneft CEO Igor Sechin failed to appear in court on Wednesday.")
  , ("igor_sechin","Igor Sechin, the chief executive of Russian oil major Rosneft, will not be able to appear in court this year to act as a witness in the trial of ex-economy minister Alexei Ulyukayev, judge Larisa Semyonova said during court hearings on Wednesday.")
  , ("half_a_million","Half a million people have signed an online petition in under 24 hours backing Uber's bid to stay on the roads of London, showing the company is turning to its tried-and-tested tactic of asking  customers for help when it locks horns with regulators.")
  , ("uber_breach","Uber Technologies Inc [UBER.UL] failed to disclose a massive breach last year that exposed the date of some 57 million users of the ride-sharing service, the company's new chief executive officer said on Tuesday.")
  , ("standard_club","British ship insurer Standard Club is setting up a new European Union subsidiary in Dublin in case Britain loses access to the single market after Brexit, becoming the second of these specialised providers to opt for Ireland in two days.")
  , ("disney_netflix","Walt Disney Co’s plan to plunge into a crowded streaming services market, dominated by pioneer Netflix Inc, could bring some initial pain, but the strength of the company’s content is expected to help it pull through in the long run.")
  , ("europe_share","European shares opened slightly higher on Wednesday, losing some of the momentum that pushed stocks in Asia and on Wall Street to new highs overnight on continued faith in synchronised economic growth around the world")
  , ("saudi_energy","Saudi Arabia’s energy minister Khalid al-Falih said on Thursday the focus of oil producers will be to continue to work on drawing down crude inventories.")
  , ("opec_stay","OPEC is likely to stay the course by keeping its current curb on oil production in place for the whole of 2018 despite potential output disruptions next year, Gulf OPEC sources said.")
  , ("fresenius_say","German healthcare group Fresenius said third-quarter adjusted net income gained 11 percent on higher drug sales and helped by the addition of Spanish hospitals chain Quironsalud.")
  , ("samsung_ai","Samsung Electronics Co Ltd said on Wednesday it would create an artificial intelligence (AI) research centre and strengthen an executive role to look for new business areas for all its three major business groups.")
  , ("disney_netflix2","Walt Disney Co (DIS.N) will stop providing new movies to Netflix Inc (NFLX.O) starting in 2019 and launch its own streaming service as the world’s biggest entertainment company tries to capture digital viewers who are dumping traditional television.")
  , ("usiminas_price","Brazilian steelmaker Usiminas Siderugicas de Minas Gerais SA is currently discussing a 25 percent steel price hike with automakers and hopes to conclude talks by the end of December, Chief Executive Sergio Leite said on Wednesday.")
  , ("petroleo_file","Brazilian state-run oil company Petroleo Brasileiro SA has filed documents related to an initial public offering of its fuel distribution unit BR Distribuidora SA, it said in a filing on Wednesday.")
  , ("kinder_morgan","Kinder Morgan Canada Ltd has complied with an order by the country's National Energy Board (NEB) to stop some work on its Trans Mountain pipeline expansion, the regulator said on Thursday.")
  , ("qualcomm_approval","U.S. smartphone chipmaker Qualcomm Inc may win European Union approval for its bid to acquire NXP Semiconductors NV by the end of the year, Bloomberg reported on Thursday, citing people familiar with the matter.")
  , ("indian_fashion","Indian fashion and retail conglomerate Future Group said on Tuesday it plans to open 10,000 member-only stores by 2022, utilizing the technology and data of Alphabet Inc's Google and Facebook Inc.")
  , ("push_momentum","European shares opened slightly higher on Wednesday, losing some of the momentum that pushed stocks in Asia and on Wall Street to new highs overnight on continued faith in synchronised economic growth around the world.")

  ]


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
  (apredata,netagger,_,_) <- loadConfig (False,False) cfg
  withFile (tcfg^.tconfig_testset) WriteMode $ \h ->
    J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
      pp <- loadJVM
      lst <- flip mapM testsets $ \(tid,txt) -> do
               dainput <- runParser pp txt
               let ner = nerDocument apredata netagger dainput
               return (Test tid txt dainput ner)
      BL.hPutStrLn h (encode lst)
