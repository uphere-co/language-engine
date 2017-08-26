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
import           Data.Range                                   (isInsideR)
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
--
import           OntoNotes.App.Analyze.Type                   (DocAnalysisInput,dainput_sents,dainput_tokss)


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




linkedMentionToTagPos :: (EntityMention Text) --  UIDCite EntityMentionUID (EL.EMInfo Text)
                      -> (TagPos TokIdx (EntityMention Text)) -- EntityMentionUID
linkedMentionToTagPos linked_mention =
  let -- uid = EL._uid linked_mention
      IRange b e = (_info linked_mention)^._1
  in TagPos (TokIdx b, TokIdx e,linked_mention)


  


prepareNETokens all =
  let mws = all ^.. traverse . sentenceWord
      mns = all ^.. traverse . sentenceNER
      unNER (NERSentence tokens) = tokens
      neTokens = concat $ map (\(x,y) -> (unNER $ sentToNER' x y)) (zip mws mns)
  in neTokens


getWikiResolvedMentions :: DocAnalysisInput 
                        -> ([(Text,NamedEntityClass)] -> [EntityMention Text])
                        -> [EntityMention Text]
getWikiResolvedMentions docinput emTagger =
  let tokens = docinput^.dainput_tokss
      linked_mentions_all =  emTagger (prepareNETokens (docinput^.dainput_sents)) -- getWikiAllMentions docinput emTagger
      input_pos = V.fromList (map (^. token_pos) $ concat tokens)
      linked_mentions_all_unfiltered = (EMP.filterEMbyPOS input_pos linked_mentions_all)
  in filter (\x -> let (_,_,pne) = _info x in case pne of Resolved _ -> True ; _ -> False) linked_mentions_all_unfiltered


{-
getWikiAllMentions :: [Sentence]
                   -> ([(Text,NamedEntityClass)] -> [EntityMention Text])
                   -> [EntityMention Text]
getWikiAllMentions sents emTagger = emTagger (prepareNETokens sents)
-}
