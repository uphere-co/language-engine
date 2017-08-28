{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module OntoNotes.App.Analyze.Format where

import           Control.Lens                            ((^.),(^?),(%~),_1,_2,_3,_5,_6,_7,_Just)
import           Control.Monad                           ((>=>))
import           Data.Function                           ((&),on)
import           Data.List                               (find,intercalate,intersperse,maximumBy,sortBy)
import           Data.Maybe                              (fromMaybe,mapMaybe)
import           Data.Monoid                             ((<>))
import           Data.Text                               (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T.IO
import           Text.PrettyPrint.Boxes                  (Box,left,hsep,text,top,vcat)
import           Text.Printf                             (printf)
import           Text.ProtocolBuffers.Basic              (Utf8)
--
import           CoreNLP.Simple.Convert                  (sentToTokens,sentToTokens')
import           CoreNLP.Simple.Type.Simplified          (Token,token_lemma,token_pos)
import           Lexicon.Format                          (formatArgPatt,formatArgPattStat,formatRoleMap)
import           Lexicon.Merge                           (constructTopPatterns,mergePatterns,patternGraph,patternRelation
                                                         ,listOfSupersetSubset,topPatterns)
import           Lexicon.Query                           (cutHistogram)
import           Lexicon.Type                            (ArgPattern(..),type RoleInstance
                                                         ,type RolePattInstance,POSVorN(..))
import           NLP.Syntax.Type                         (Voice)                 
import 	       	 NLP.Type.CoreNLP                        (Token,token_lemma,token_pos)
import qualified WikiEL                        as WEL
import           WikiEL.EntityLinking                    (UIDCite(..),EMInfo,EntityMentionUID,_emuid)
import qualified WikiEL.EntityLinking          as EL
import           WikiEL.WikiNamedEntityTagger                 (resolveNEs,getStanfordNEs,parseStanfordNE,namedEntityAnnotator,resolvedUID)

--
import           OntoNotes.App.Util                      (CharIdx,TokIdx,TagPos(..),SentItem
                                                         ,addTag,convertTagPosFromTokenToChar
                                                         ,underlineText)
import           OntoNotes.App.WikiEL                    (linkedMentionToTagPos)



chooseFrame :: [(Text,Text,Int,Text,Text,Text,Text)] -> Maybe (Text,Text,Int,Text,Text,Text,Text)
chooseFrame [] = Nothing
chooseFrame xs = Just (maximumBy (compare `on` (^._3)) xs)


formatLemmaPOS :: Token -> String
formatLemmaPOS t = printf "%10s %5s" (t^.token_lemma) (show (t^.token_pos))


formatTimex :: (SentItem CharIdx, [TagPos CharIdx (Maybe Text)]) -> [Text]
formatTimex (s,a) = (underlineText (const "") (s^._2) (s^._3) a) ++ ["----------"] ++ [T.pack (show a)]


showTimex :: (SentItem CharIdx, [TagPos CharIdx (Maybe Text)]) -> IO ()
showTimex (s,a) = T.IO.putStrLn (T.intercalate "\n" (formatTimex (s,a)))



getFormatTimex' :: (SentItem CharIdx,[TagPos CharIdx (Maybe Text)]) -> [Text]
getFormatTimex' (s,a) = (underlineText (const "") (s^._2) (s^._3) a) ++ ["----------"] ++ [T.pack (show a)]

showFormatTimex' :: (SentItem CharIdx,[TagPos CharIdx (Maybe Text)]) -> IO ()
showFormatTimex' (s,a) = T.IO.putStrLn (T.intercalate "\n" (getFormatTimex' (s,a)))




formatSense :: (Text,Text,Int,Text,Text,Text,Text) -> String
formatSense (sgrp,sn,num,txt_def,txt_frame,txt_fecore,txt_feperi) =
  printf "%2s.%-6s (%4d cases) | %-40s | %-20s | %-40s      ------       %-30s " sgrp sn num txt_def txt_frame txt_fecore txt_feperi


formatSenses :: Bool  -- ^ doesShowOtherSense
             -> [RoleInstance]
             -> [RolePattInstance Voice]
             -> Text
             -> [(Text,Text,Int,Text,Text,Text,Text)]
             -> String
formatSenses doesShowOtherSense rolemap subcats lma lst
  = let t = chooseFrame lst
    in "Top frame: "
       ++ printf " %-20s | %-40s      ------      %-30s\n"
            (fromMaybe "" (t^?_Just._5))
            (fromMaybe "" (t^?_Just._6))
            (fromMaybe "" (t^?_Just._7))
       ++ "--------------------------------------------------------------------------------------------------\n"
       ++ fromMaybe ""
            (do t1 <- t^?_Just._1
                t2 <- t^?_Just._2
                let sid = (lma,Verb, t1<>"."<>t2)
                rm <- find (\rm -> rm^._1 == sid) rolemap
                let msubcat =find ((== sid) . (^._1)) subcats
                let margpattstr = do
                      subcat <- msubcat
                      let toppatts_cut = cutHistogram 0.9 (constructTopPatterns (subcat^._2))
                      return (formatArgPattStat toppatts_cut)
                return (formatRoleMap (rm^._2) ++ maybe "" ("\n- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"<>) margpattstr)
            )
       ++ "\n--------------------------------------------------------------------------------------------------\n"
       ++ if doesShowOtherSense
          then "\n\n\n*********************************************\n" ++ intercalate "\n" (map formatSense lst)
          else ""



formatLinkedMention Cite {..} = printf "%3d: (-> %3d) %s " (EL._emuid _uid) (EL._emuid _ref) (formatEMInfo _info)
formatLinkedMention Self {..} = printf "%3d:          %s " (EL._emuid _uid)                  (formatEMInfo _info)


formatIndexedTimex (c,mtxt)   = printf "%3s:          %s " (T.singleton c) (fromMaybe "" mtxt)

formatTaggedSentences :: (a -> Text) ->  [(SentItem CharIdx,[TagPos CharIdx a])] -> Box 
formatTaggedSentences f sents_tagged =
  let txts = concatMap (\(s,a) -> underlineText f (s^._2) (s^._3) a) sents_tagged
  in vcat top $ map (text . T.unpack) txts 


formatPreNE tag = case resolvedUID tag of
                    Left e -> "unresolved"
                    Right i -> show i


formatEMInfo :: EL.EMInfo Text -> String
formatEMInfo em@(_,ws,tag) = printf "%-25s %-20s" (WEL.entityName em) (formatPreNE tag)






formatTagged :: [[Maybe Token]] -> [SentItem CharIdx] -> [TagPos TokIdx (Either (EL.EntityMention Text) (Char,(Maybe Text)))] -> Box
formatTagged mtokenss sentitems tlst =
  let toks = concatMap (map snd . sentToTokens') mtokenss
      clst = mapMaybe (convertTagPosFromTokenToChar toks) tlst
      sents_tagged = map (addTag clst) sentitems
      doc1 = formatTaggedSentences (either (T.pack . show . _emuid . EL._uid) (T.singleton . (^._1))) sents_tagged
      doc2 = vcat top . intersperse (text "") . map (text. either formatLinkedMention formatIndexedTimex) $ map (\(TagPos (_,_,x)) -> x) clst
  in hsep 10 left [doc1,doc2]
