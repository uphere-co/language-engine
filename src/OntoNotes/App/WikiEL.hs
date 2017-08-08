{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module OntoNotes.App.WikiEL where

import           Control.Lens                                 ((^.),_1,_2,_3)
import           Control.Monad                                (guard)
import           Data.Map                                     (Map)
import qualified Data.Map                               as M
import           Data.Text                                    (Text)
import qualified Data.Text                              as T
import           System.FilePath                              ((</>))
import           Text.PrettyPrint.Boxes                       (text,top,vcat)
import           Text.Printf                                  (printf)
--
import           CoreNLP.Simple.Type.Simplified
import           WikiEL                                       (loadEMtagger)
import           WikiEL.CoreNLP                               (parseNEROutputStr)
import           WikiEL.WikiEntityTagger                      (loadWETagger,wikiAnnotator)
import           WikiEL.WikiEntityClass                       (fromFiles,getNEClass)
import           WikiEL.WikiNamedEntityTagger                 (resolveNEs,getStanfordNEs,parseStanfordNE,namedEntityAnnotator,resolvedUID)
import           WikiEL.WikiNamedEntityTagger                 (PreNE(..),resolveNEClass)
import           WikiEL.EntityLinking                         (EntityMentionUID,EntityMention(..),UIDCite(..),entityLinking,entityLinkings,buildEntityMentions,entityUID)
import           WikiEL.ETL.LoadData
-- For testing:
import           WikiEL.Misc                                  (IRange(..),untilOverlapOrNo,untilNoOverlap,relativePos, isContain,subVector)
import qualified NLP.Type.NamedEntity          as N
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
import           OntoNotes.App.Util                   (TagPos(..),underlineText)


groupupheredir' = "/scratch/groups/uphere"
wikinerdir' = groupupheredir </> "wiki-ner"

listedCompanyFile' = groupupheredir </> "enwiki/companies"
newsFileTxt' = wikinerdir </> "data/article.amazon_nike.txt"
-- rawNewsFile3 = wikinerdir </> "data/article.amazon_nike.ptb"
-- nerNewsFile3 = wikinerdir </> "data/article.amazon_nike.ner"
reprFile'     = EntityReprFile (wikinerdir </> "data/uid")
orgItemFile'  = ItemIDFile (wikinerdir </> "data/ne.org")
personItemFile' = ItemIDFile (wikinerdir </> "data/ne.person")
brandItemFile'  = ItemIDFile (wikinerdir </> "data/ne.brand")
wordnetMappingFile' = WordNetMappingFile (wikinerdir </> "data/page_id.wiki_id.wordnet.tsv")



groupupheredir = "/data/groups/uphere/data/Wiki"
wikinerdir = groupupheredir </> "wiki-ner"

listedCompanyFile = groupupheredir </> "companies"
newsFileTxt = wikinerdir </> "data/article.amazon_nike.txt"
-- rawNewsFile3 = wikinerdir </> "data/article.amazon_nike.ptb"
-- nerNewsFile3 = wikinerdir </> "data/article.amazon_nike.ner"
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

linkedMentionToTagPOS :: [Token]
                      -> UIDCite EntityMentionUID (EL.EMInfo Text)
                      -> Maybe (TagPos EntityMentionUID)
linkedMentionToTagPOS toks linked_mention = do
  let uid = EL._uid linked_mention
      IRange b e = (_info linked_mention)^._1
      matched_toks = filter (\tok -> (tok^.token_tok_idx_range) `isInsideR` (b,e)) toks
  guard ((not.null) matched_toks)
  let cb = (head matched_toks)^.token_char_idx_range._1
      ce = (last matched_toks)^.token_char_idx_range._2
      tagpos = (cb+1,ce,uid)
  return tagpos


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

