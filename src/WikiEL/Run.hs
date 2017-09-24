module WikiEL.Run where

import           Control.Lens
import           Data.Maybe
--
-- import           CoreNLP.Simple.Convert
import           Data.Text                             (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T.IO
import qualified Data.Vector                    as V
import           NLP.Type.CoreNLP
import           NLP.Type.NamedEntity
import           NLP.Type.PennTreebankII
import           System.FilePath                          ((</>))
import           WikiEL.EntityLinking
import qualified WikiEL.EntityMentionPruning    as EMP
import           WikiEL.Misc
import           WikiEL.WikiNamedEntityTagger
--
import           WikiEL.Type.FileFormat
import qualified WikiEL.WikiEntityClass        as WC
import qualified WikiEL.WikiNamedEntityTagger  as WNET
import qualified WikiEL                        as WEL


prepareWNP :: [Sentence] -> [(Text,NamedEntityClass,POSTag)]
prepareWNP sents =
  let ws = catMaybes $ sents ^.. traverse . sentenceWord . traverse
      ns' = catMaybes $ sents ^.. traverse . sentenceNER . traverse
      ns = map (\x -> (fromMaybe (error (show x)) . classify) x) ns'
      ts = catMaybes $ sents ^.. traverse . sentenceToken . traverse
      ps = map (^. token_pos) ts
  in (zip3 ws ns ps)


mkConstraintFromWikiEL :: [EntityMention Text] -> [(Int,Int)]
mkConstraintFromWikiEL wikiel = map (\x -> let irange = entityIRange x in (beg irange, end irange)) $ wikiel

runEL :: [Sentence]
      -> ([(Text,NamedEntityClass,POSTag)] -> [EntityMention Text])
      -> ([EntityMention Text] -> [EntityMention Text])
      -> [EntityMention Text]
runEL sents tagger entityResolve =
  let wnps = prepareWNP sents
      linked_mentions = tagger wnps
  in entityResolve linked_mentions


loadWikiData :: IO ([(Text, NamedEntityClass,POSTag)] -> [EntityMention Text], [EntityMention Text] -> [EntityMention Text])
loadWikiData = do
  edges  <- WEL.loadAndSortEdges graphFilesG
  uidTag <- WEL.loadFiles classFilesG
  titles <- WEL.loadWikipageMapping wikiTitleMappingFileG
  tagger <- WEL.loadFEMtagger reprFileG classFilesG
  let entityResolve = WEL.disambiguateMentions edges uidTag titles
  return (tagger,entityResolve)


reprFileTinyG :: EntityReprFile
reprFileTinyG       = EntityReprFile (globalData </> "wiki-ner/data/wikidata.test.entities")

orgItemFileG,personItemFileG,brandItemFileG,locationItemFileG,occupationItemFileG,humanRuleItemFileG,buildingItemFileG :: ItemIDFile
orgItemFileG        = ItemIDFile (globalData </> "wiki-ner/data/ne.org")
personItemFileG     = ItemIDFile (globalData </> "wiki-ner/data/ne.person")
brandItemFileG      = ItemIDFile (globalData </> "wiki-ner/data/ne.brand")
locationItemFileG   = ItemIDFile (globalData </> "wiki-ner/data/ne.loc")
occupationItemFileG = ItemIDFile (globalData </> "wiki-ner/data/ne.occupation")
humanRuleItemFileG  = ItemIDFile (globalData </> "wiki-ner/data/ne.human_rule")
buildingItemFileG   = ItemIDFile (globalData </> "wiki-ner/data/ne.building")

classFilesG :: [(WC.ItemClass,ItemIDFile)]
classFilesG = [ (WC.personClass, personItemFileG)
              , (WC.orgClass,    orgItemFileG)
              , (WC.brandClass,  brandItemFileG)
              , (WC.occupationClass, occupationItemFileG)
              , (WC.locationClass,   locationItemFileG)
              , (WC.humanRuleClass,  humanRuleItemFileG)
              , (WC.buildingClass,   buildingItemFileG)
              ]

reprFileG :: EntityReprFile
reprFileG     = EntityReprFile (globalData </> "wiki-ner/data/names")

-- Full data

wikiTitleMappingFileG :: WikiTitleMappingFile
wikiTitleMappingFileG = WikiTitleMappingFile (globalData </> "wiki-ner/wiki_id.page_title.txt")


wordnetMappingFileG :: WordNetMappingFile
wordnetMappingFileG = WordNetMappingFile (globalData </> "wiki-ner/page_id.wiki_id.wordnet.tsv")


propertyNameFileG :: PropertyNameFile
propertyNameFileG = PropertyNameFile (globalData </> "wiki-ner/properties.tsv")

listedCompanyFileG :: FilePath
listedCompanyFileG = globalData </> "enwiki/companies"


graphFilesG :: FilePath
graphFilesG = globalData </> "wiki-ner/interlinks.filtered"


globalData :: FilePath
globalData = "/data/groups/uphere/data/Wiki"
