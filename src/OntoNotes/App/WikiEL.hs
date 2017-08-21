{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module OntoNotes.App.WikiEL where

import           Control.Lens                                 ((^.),(^..),_1,_2,_3,_4)
import           Control.Monad                                (guard)
import           Data.Map                                     (Map)
import qualified Data.Map                               as M
import           Data.Text                                    (Text)
import qualified Data.Text                              as T
import qualified Data.Vector                            as V
import           System.FilePath                              ((</>))
import           Text.PrettyPrint.Boxes                       (Box,text,top,vcat)
import           Text.Printf                                  (printf)
--
import           CoreNLP.Simple.Convert                       (sentToNER')
import           CoreNLP.Simple.Type.Simplified
import           NLP.Type.NamedEntity                         (NamedEntityClass)
import           WikiEL                                       (loadEMtagger)
import           WikiEL.CoreNLP                               (parseNEROutputStr)
import           WikiEL.WikiEntityTagger                      (loadWETagger,wikiAnnotator)
import           WikiEL.WikiEntityClass                       (fromFiles,getNEClass)
import           WikiEL.WikiNamedEntityTagger                 (resolveNEs,getStanfordNEs,parseStanfordNE,namedEntityAnnotator,resolvedUID)
import           WikiEL.WikiNamedEntityTagger                 (PreNE(..),resolveNEClass)
import           WikiEL.EntityLinking                         (EntityMentionUID,EntityMention(..),UIDCite(..)
                                                              ,entityLinking,entityLinkings,buildEntityMentions,entityUID)
import qualified WikiEL.EntityMentionPruning   as EMP
import           WikiEL.ETL.LoadData
-- For testing:
import           WikiEL.Misc                                  (IRange(..),untilOverlapOrNo,untilNoOverlap,relativePos, isContain,subVector)
import qualified NLP.Type.NamedEntity          as N
import           NLP.Type.PennTreebankII                      (PennTree)
import           PropBank.Util                                (isInsideR)
import qualified WikiEL.WikiEntityClass        as WC
import           WikiEL.Type.Wikidata
import           WikiEL.Type.Wikipedia
import           WikiEL.Type.WordNet
import           WikiEL.Type.Equity
import           WikiEL.Type.FileFormat
import           WikiEL.ETL.Parser
import           WikiEL.WordNet
import qualified WikiEL.EntityLinking          as EL
import qualified WikiEL.Type.FileFormat        as F
import qualified WikiEL                        as WEL
--
import           OntoNotes.App.Util                           (CharIdx(..),SentItem,TagPos(..)
                                                              ,TokIdx(..)
                                                              ,convertRangeFromTokenToChar
                                                              ,underlineText)


groupupheredir' = "/scratch/groups/uphere"

wikinerdir' = groupupheredir </> "wiki-ner"

listedCompanyFile' = groupupheredir </> "enwiki/companies"

newsFileTxt' = wikinerdir </> "data/article.amazon_nike.txt"

reprFile'     = EntityReprFile (wikinerdir </> "data/uid")

orgItemFile'  = ItemIDFile (wikinerdir </> "data/ne.org")

personItemFile' = ItemIDFile (wikinerdir </> "data/ne.person")

brandItemFile'  = ItemIDFile (wikinerdir </> "data/ne.brand")

wordnetMappingFile' = WordNetMappingFile (wikinerdir </> "data/page_id.wiki_id.wordnet.tsv")



groupupheredir = "/data/groups/uphere/data/Wiki"

wikinerdir = groupupheredir </> "wiki-ner"

listedCompanyFile = groupupheredir </> "companies"

newsFileTxt = wikinerdir </> "data/article.amazon_nike.txt"

reprFile     = EntityReprFile (wikinerdir </> "data/uid")

orgItemFile  = ItemIDFile (wikinerdir </> "data/ne.org")

personItemFile = ItemIDFile (wikinerdir </> "data/ne.person")

brandItemFile  = ItemIDFile (wikinerdir </> "data/ne.brand")

wordnetMappingFile = WordNetMappingFile (wikinerdir </> "data/page_id.wiki_id.wordnet.tsv")
 
getOrgs :: EntityMention a -> Maybe (EntityMentionUID, ItemID)
getOrgs (EL.Self muid (_,_, Resolved (wuid, N.Org))) = Just (muid, wuid)
getOrgs (EL.Cite muid _ (_,_, Resolved (wuid, N.Org))) = Just (muid, wuid)
getOrgs _ = Nothing


getCompanySymbol :: Map ItemID Symbol -> (EntityMentionUID, ItemID) -> Maybe (EntityMentionUID , ItemID, Symbol)
getCompanySymbol tikcerMap (mentionUID, itemID) = result
  where
    result = case M.lookup itemID tikcerMap of
      Just symbol -> Just (mentionUID, itemID, symbol)
      Nothing     -> Nothing  




linkedMentionToTagPos -- :: [Token]
                      :: UIDCite EntityMentionUID (EL.EMInfo Text)
                      -- -> Maybe (TagPos CharIdx EntityMentionUID)
                      -> (TagPos TokIdx EntityMentionUID)
linkedMentionToTagPos linked_mention =
  let uid = EL._uid linked_mention
      IRange b e = (_info linked_mention)^._1
  in (TokIdx b, TokIdx e,uid)


  

formatTaggedSentences :: [(SentItem CharIdx,[TagPos CharIdx EntityMentionUID])] -> Box 
formatTaggedSentences sents_tagged =
  let txts = concatMap (\(s,a) -> underlineText (T.pack . show . EL._emuid) (s^._2) (s^._3) a) sents_tagged
  in vcat top $ map (text . T.unpack) txts 


formatPreNE tag = case resolvedUID tag of
                    Left e -> "unresolved"
                    Right i -> show i


formatEMInfo :: EL.EMInfo Text -> String
formatEMInfo em@(_,ws,tag) = printf "%-25s %-20s" (WEL.entityName em) (formatPreNE tag)


formatLinkedMention Cite {..} = printf "%3d: (-> %3d) %s " (EL._emuid _uid) (EL._emuid _ref) (formatEMInfo _info)
formatLinkedMention Self {..} = printf "%3d:          %s " (EL._emuid _uid)                  (formatEMInfo _info)


prepareNETokens all =
  let mws = all ^.. traverse . sentenceWord
      mns = all ^.. traverse . sentenceNER
      unNER (NERSentence tokens) = tokens
      neTokens = concat $ map (\(x,y) -> (unNER $ sentToNER' x y)) (zip mws mns)
  in neTokens


getWikiResolvedMentions :: ([Sentence], [Maybe SentenceIndex], [SentItem CharIdx], [[Token]]
                           ,[Maybe PennTree]
                           ,[Dependency]
                           ,Maybe [(SentItem CharIdx, [TagPos CharIdx (Maybe Text)])])
                        -> ([(Text,NamedEntityClass)] -> [EntityMention Text])
                        -> [EntityMention Text]
getWikiResolvedMentions loaded emTagger =
  let tokens = loaded^._4
      linked_mentions_all = getWikiAllMentions loaded emTagger
      input_pos = V.fromList (map (^. token_pos) $ concat tokens)
      linked_mentions_all_unfiltered = (EMP.filterEMbyPOS input_pos linked_mentions_all)
  in filter (\x -> let (_,_,pne) = _info x in case pne of Resolved _ -> True ; _ -> False) linked_mentions_all_unfiltered


getWikiAllMentions :: ([Sentence]
                      ,[Maybe SentenceIndex]
                      ,[SentItem CharIdx]
                      ,[[Token]]
                      ,[Maybe PennTree]
                      ,[Dependency]
                      ,Maybe [(SentItem CharIdx, [TagPos CharIdx (Maybe Text)])])
                   -> ([(Text,NamedEntityClass)] -> [EntityMention Text])
                   -> [EntityMention Text]
getWikiAllMentions loaded emTagger =
  let neTokens = prepareNETokens (loaded^._1)
      linked_mentions_all = emTagger neTokens
  in linked_mentions_all
