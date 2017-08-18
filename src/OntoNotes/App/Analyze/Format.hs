{-# LANGUAGE OverloadedStrings #-}

module OntoNotes.App.Analyze.Format where

import           Control.Lens                            ((^.),(^?),(%~),_1,_2,_3,_5,_6,_7,_Just)
import           Data.Function                           ((&),on)
import           Data.List                               (find,intercalate,intersperse,maximumBy)
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
import           WikiEL.EntityLinking                    (UIDCite(..),EMInfo,EntityMentionUID)
--
import           OntoNotes.App.Util                      (TagPos,SentItem,addTag,underlineText)
import           OntoNotes.App.WikiEL                    (formatLinkedMention,formatTaggedSentences,linkedMentionToTagPOS)
import           OntoNotes.Format                        (formatArgPatt,formatRoleMap)
import           OntoNotes.Type.ArgTable



chooseFrame :: [(Text,Text,Int,Text,Text,Text,Text)] -> Maybe (Text,Text,Int,Text,Text,Text,Text)
chooseFrame [] = Nothing
chooseFrame xs = Just (maximumBy (compare `on` (^._3)) xs)


formatLemmaPOS :: Token -> String
formatLemmaPOS t = printf "%10s %5s" (t^.token_lemma) (show (t^.token_pos))


formatTimex :: (SentItem,[TagPos (Maybe Utf8)]) -> [Text]
formatTimex (s,a) = (underlineText (const "") (s^._2) (s^._3) a) ++ ["----------"] ++ [T.pack (show a)]


showTimex :: (SentItem,[TagPos (Maybe Utf8)]) -> IO ()
showTimex (s,a) = T.IO.putStrLn (T.intercalate "\n" (formatTimex (s,a)))



getFormatTimex' :: (SentItem,[TagPos (Maybe Text)]) -> [Text]
getFormatTimex' (s,a) = (underlineText (const "") (s^._2) (s^._3) a) ++ ["----------"] ++ [T.pack (show a)]

showFormatTimex' :: (SentItem,[TagPos (Maybe Text)]) -> IO ()
showFormatTimex' (s,a) = T.IO.putStrLn (T.intercalate "\n" (getFormatTimex' (s,a)))




formatSense :: (Text,Text,Int,Text,Text,Text,Text) -> String
formatSense (sgrp,sn,num,txt_def,txt_frame,txt_fecore,txt_feperi) = 
  printf "%2s.%-6s (%4d cases) | %-40s | %-20s | %-40s      ------       %-30s " sgrp sn num txt_def txt_frame txt_fecore txt_feperi


formatSenses :: Bool  -- ^ doesShowOtherSense
             -> [((Text,Text), [(Text,Text)])]
             -> [((Text,Text),[(ArgPattern Text,Int)])]
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
       ++ "--------------------------------------------------------------------------------------------------\n"                 -- ++ show subcats
       ++ fromMaybe ""
            (do t1 <- t^?_Just._1
                t2 <- t^?_Just._2
                let sid = (lma, t1<>"."<>t2)
                rm <- find (\rm -> rm^._1 == sid) rolemap
                let sid' = (lma<>"-v",t2)
                    msubcat =find ((== sid') . (^._1)) subcats
                let margpattstr = do
                      subcat <- msubcat
                      return $ intercalate "\n" $ flip map (Prelude.take 5 (subcat^._2)) $ \(patt,n) ->
                                 printf "%s     #count: %5d" (formatArgPatt patt) (n :: Int)
                return (formatRoleMap (rm^._2) ++ maybe "" ("\n- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"<>) margpattstr)
            )
       ++ "\n--------------------------------------------------------------------------------------------------\n"
       ++ if doesShowOtherSense
          then "\n\n\n*********************************************\n" ++ intercalate "\n" (map formatSense lst)
          else ""



formatNER psents sentitems linked_mentions_resolved =
  let toks = concatMap (map snd . sentToTokens) psents
      tags = mapMaybe (linkedMentionToTagPOS toks) linked_mentions_resolved
      sents_tagged = map (addTag tags) sentitems
      doc1 = formatTaggedSentences sents_tagged
      doc2 = vcat top . intersperse (text "") . map (text.formatLinkedMention) $ linked_mentions_resolved
  in hsep 10 left [doc1,doc2]


      
formatNER' :: [[Maybe Token]] -> [SentItem] -> [UIDCite EntityMentionUID (EMInfo Text)] -> Box
formatNER' psents sentitems linked_mentions_resolved =
  let toks = concatMap (map snd . sentToTokens') psents
      tags = mapMaybe (linkedMentionToTagPOS toks) linked_mentions_resolved
      sents_tagged = map (addTag tags) sentitems
      doc1 = formatTaggedSentences sents_tagged
      doc2 = vcat top . intersperse (text "") . map (text.formatLinkedMention) $ linked_mentions_resolved
  in hsep 10 left [doc1,doc2]

