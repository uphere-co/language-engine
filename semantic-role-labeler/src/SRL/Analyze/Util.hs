{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- the functions in this module will be relocated to a more common package like textview
--
module SRL.Analyze.Util where

import           Control.Lens
import           Control.Monad                                 (guard)
import           Data.Maybe                                    (fromJust,mapMaybe)
import           Data.Text                                     (Text)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import qualified CoreNLP.Proto.CoreNLPProtos.Timex     as Tmx
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as T
import           CoreNLP.Simple.Convert                        (cutf8)
import           Data.Range                                    (isInsideR)
import           NLP.Type.CoreNLP
import           NLP.Type.TagPos
import           Text.Annotation.Type
import           Text.Annotation.Util.Doc
import           Text.Annotation.View


addText :: Text -> (SentIdx,BeginEnd CharIdx) -> SentItem CharIdx
addText txt (n,(b,e)) = (n,(b,e),slice (unChIdx (b-1)) (unChIdx e) txt)

addTag :: (Ord i) => [TagPos i a] -> SentItem i -> (SentItem i,[TagPos i a])
addTag lst i@(_,(b,e),_) = (i,filter check lst)
  where check (TagPos (b',e',_)) = b' >= b && e' <= e


underlineText :: (a -> Text) -> BeginEnd CharIdx -> Text -> [TagPos CharIdx a] -> [Text]
underlineText lblf (b0,_e0) txt taglst =
  let adjf (TagPos (b,e,z)) = (z,unChIdx (b-b0+1),unChIdx (e-b0+1))
      ann = AnnotText (tagText (map adjf taglst) txt)
      xss = lineSplitAnnot Nothing 80 ann
      ls = do xs <- xss
              x <- xs
              underlineAnnotWithLabel (fmap lblf) x
  in ls


getSentenceOffsets :: [S.Sentence] -> [(SentIdx,BeginEnd CharIdx)]
getSentenceOffsets psents =
  zip ([1..] :: [Int]) $ flip map psents $ \s ->
    let b = fromJust $ fromJust $ firstOf (S.token . traverse . TK.beginChar) s
        e = fromJust $ fromJust $ lastOf  (S.token . traverse . TK.endChar) s
    in (fromIntegral b+1,fromIntegral e)


listTimexToTagPos :: T.ListTimex -> [TagPos TokIdx (Maybe Text)]
listTimexToTagPos tmxs = tmxs^..
                           T.timexes . traverse
                           . to (\t -> TagPos (fi (t^.T.tokenBegin), fi (t^.T.tokenEnd), t^?T.timex.Tmx.value._Just.to cutf8))
  where fi = fromIntegral                                                                     


addSUTime :: [SentItem CharIdx]
          -> [Token]
          -> T.ListTimex
          -> [(SentItem CharIdx,[TagPos CharIdx (Maybe Text)])]
addSUTime sents toks tmxs =
  let tagposs = listTimexToTagPos tmxs
      tagposs' = flip mapMaybe tagposs $ \(TagPos (i,j,x)) -> do
                   (cstart,cend) <- convertRangeFromTokenToChar toks (i,j)
                   return $ TagPos (cstart,cend,x)
  in (filter (not.null.(^._2)) . map (addTag tagposs')) sents


convertRangeFromTokenToChar :: [Token] -> (TokIdx,TokIdx) -> Maybe (CharIdx,CharIdx)
convertRangeFromTokenToChar toks (TokIdx b,TokIdx e) = do
  let matched_toks = filter (\tok -> (tok^.token_tok_idx_range) `isInsideR` (b,e)) toks
  guard ((not.null) matched_toks)
  let cb = (head matched_toks)^.token_char_idx_range._1
      ce = (last matched_toks)^.token_char_idx_range._2
  return (ChIdx (cb+1),ChIdx ce)
        

convertTagPosFromTokenToChar :: [Token] -> TagPos TokIdx a -> Maybe (TagPos CharIdx a)
convertTagPosFromTokenToChar toks (TagPos (tb,te,x)) = 
  convertRangeFromTokenToChar toks (tb,te) >>= \(cs,ce) -> return (TagPos (cs,ce,x))
