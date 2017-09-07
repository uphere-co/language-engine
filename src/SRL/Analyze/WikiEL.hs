{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SRL.Analyze.WikiEL where

import           Control.Lens                                 ((^.),(^..),_1)
import           Data.Either                                  (lefts)
import           Data.Foldable                                (toList)
import           Data.Map                                     (Map)
import qualified Data.Map                               as M
import           Data.Text                                    (Text)
import qualified Data.Vector                            as V
import           System.FilePath                              ((</>))
--
import           CoreNLP.Simple.Convert                       (sentToNER')
import           NLP.Type.NamedEntity                         (NamedEntityClass)

import           WikiEL.Convert                               (getNEFromEntityMention,getRangeFromEntityMention)
import           WikiEL.WikiNamedEntityTagger                 (PreNE(..))
import           WikiEL.EntityLinking                         (EntityMentionUID,EntityMention,UIDCite(..))
import qualified WikiEL.EntityMentionPruning   as EMP
-- For testing:
import           WikiEL.Misc                                  (IRange(..))
import           NLP.Type.CoreNLP
import qualified NLP.Type.NamedEntity          as N
import           NLP.Type.TagPos                              (TagPos(..),TokIdx(..))
import           WikiEL.Type.Wikidata
import           WikiEL.Type.Equity
import           WikiEL.Type.FileFormat
import qualified WikiEL.EntityLinking          as EL
--
import           SRL.Analyze.Type


groupupheredir' :: FilePath
groupupheredir' = "/scratch/groups/uphere"


wikinerdir' :: FilePath
wikinerdir' = groupupheredir </> "wiki-ner"


listedCompanyFile' :: FilePath
listedCompanyFile' = groupupheredir </> "enwiki/companies"


newsFileTxt' :: FilePath
newsFileTxt' = wikinerdir </> "data/article.amazon_nike.txt"


reprFile' :: EntityReprFile
reprFile'     = EntityReprFile (wikinerdir </> "data/uid")


orgItemFile' :: ItemIDFile
orgItemFile'  = ItemIDFile (wikinerdir </> "data/ne.org")


personItemFile' :: ItemIDFile
personItemFile' = ItemIDFile (wikinerdir </> "data/ne.person")


brandItemFile' :: ItemIDFile
brandItemFile'  = ItemIDFile (wikinerdir </> "data/ne.brand")


wordnetMappingFile' :: WordNetMappingFile
wordnetMappingFile' = WordNetMappingFile (wikinerdir </> "data/page_id.wiki_id.wordnet.tsv")


groupupheredir :: FilePath
groupupheredir = "/data/groups/uphere/data/Wiki"


wikinerdir :: FilePath
wikinerdir = groupupheredir </> "wiki-ner"


listedCompanyFile :: FilePath
listedCompanyFile = groupupheredir </> "companies"


newsFileTxt :: FilePath
newsFileTxt = wikinerdir </> "data/article.amazon_nike.txt"


reprFile     :: EntityReprFile 
reprFile     = EntityReprFile (wikinerdir </> "data/uid")


orgItemFile :: ItemIDFile
orgItemFile  = ItemIDFile (wikinerdir </> "data/ne.org")


personItemFile :: ItemIDFile
personItemFile = ItemIDFile (wikinerdir </> "data/ne.person")


brandItemFile :: ItemIDFile
brandItemFile  = ItemIDFile (wikinerdir </> "data/ne.brand")


locationItemFile :: ItemIDFile
locationItemFile = ItemIDFile (wikinerdir </> "data/ne.loc")


occupationItemFile :: ItemIDFile
occupationItemFile = ItemIDFile (wikinerdir </> "data/ne.occupation")


humanRuleItemFile :: ItemIDFile
humanRuleItemFile = ItemIDFile (wikinerdir </> "data/ne.human_rule")


buildingItemFile :: ItemIDFile
buildingItemFile = ItemIDFile (wikinerdir </> "data/ne.building")


wordnetMappingFile :: WordNetMappingFile
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




linkedMentionToTagPos :: (EntityMention Text)
                      -> (TagPos TokIdx (EntityMention Text))
linkedMentionToTagPos linked_mention =
  let IRange b e = (_info linked_mention)^._1
  in TagPos (TokIdx b, TokIdx e,linked_mention)

prepareNETokens :: [Sentence] -> [(Text,NamedEntityClass)]
prepareNETokens sents =
  let mws = sents ^.. traverse . sentenceWord
      mns = sents ^.. traverse . sentenceNER
      unNER (NERSentence tokens) = tokens
      neTokens = concat $ map (\(x,y) -> (unNER $ sentToNER' x y)) (zip mws mns)
  in neTokens


getWikiResolvedMentions :: ([(Text,NamedEntityClass)] -> [EntityMention Text])
                        -> [Sentence]
                        -> [Token]
                        -> [EntityMention Text]
getWikiResolvedMentions emTagger sents tokens =
  let linked_mentions_all =  emTagger (prepareNETokens sents) -- getWikiAllMentions docinput emTagger
      input_pos = V.fromList (map (^. token_pos) tokens)
      linked_mentions_all_unfiltered = (EMP.filterEMbyPOS input_pos linked_mentions_all)
  in filter (\x -> let (_,_,pne) = _info x in case pne of Resolved _ -> True ; _ -> False) linked_mentions_all_unfiltered


adjustWikiRange :: (Int,Int) -> (Int,Int)
adjustWikiRange (a,b) = (a,b-1)



mkWikiList :: DocStructure -> [((Int, Int), Text)]
mkWikiList dstr =
  let tagposs = toList (dstr ^. ds_mergedtags)
      wikiel  = lefts $ map (\(TagPos (_,_,e)) -> e) tagposs
      wikilst = map (\w -> (adjustWikiRange $ getRangeFromEntityMention w,getNEFromEntityMention w)) wikiel
  in wikilst
      


{-
getWikiAllMentions :: [Sentence]
                   -> ([(Text,NamedEntityClass)] -> [EntityMention Text])
                   -> [EntityMention Text]
getWikiAllMentions sents emTagger = emTagger (prepareNETokens sents)
-}
