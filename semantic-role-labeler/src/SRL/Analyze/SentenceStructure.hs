{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module SRL.Analyze.SentenceStructure where

import           Control.Applicative                       (many)
import           Control.Lens                              ((^.),(^..),(^?),_1,_2,_3,to,_head,_last)
import           Control.Monad.State.Lazy                  (evalState)
import           Control.Monad.Trans.Except                (runExceptT)
import           Data.Bifunctor                            (first)
import           Data.Either                               (lefts,rights)
import           Data.Foldable                             (toList)
import           Data.IntMap                               (IntMap)
import qualified Data.IntMap                       as IM
import           Data.List                                 (zip5)
import           Data.Maybe                                (catMaybes,fromMaybe,mapMaybe)
import           Data.Monoid                               ((<>))
import qualified Data.Text                         as T
import           Data.Text                                 (Text)
import qualified Data.Vector                       as V
--
import           CompanyEL.Type                            (CompanyInfo(..),ticker)
import           CoreNLP.Simple.Convert                    (mkLemmaMap)
import           Data.Range                                (elemIsStrictlyInsideR)
import           NLP.Syntax                                (syntacticAnalysis)
import           NLP.Syntax.Verb                           (verbPropertyFromPennTree)
import           NLP.Syntax.Type.Verb                      (VerbProperty,vp_lemma,vp_index)
import           NLP.Syntax.Type.XBar                      (MarkType(..),Zipper,PreAnalysis,lemmaList,pennTree)
import           NLP.Syntax.Util                           (mkPreAnalysis)
import qualified NLP.Type.NamedEntity              as N
import           NLP.Type.CoreNLP                          ( SentenceIndex, Token
                                                           , sentenceToken
                                                           , sentenceLemma
                                                           , sent_tokenRange
                                                           , token_text
                                                           , token_tok_idx_range
                                                           )
import           NLP.Type.PennTreebankII                   (Lemma(..),PennTree)
import           NLP.Type.TagPos                           (TagPos(..),TokIdx(..),mergeTagPos)
import           Text.Search.New.ParserCustom              (pTreeAdvGBy)
import           WikiEL.Tagger                             ( orgClass
                                                           , personClass
                                                           , brandClass
                                                           )
import           WikiEL.Type                               ( EntityMention
                                                           , EntityMentionUID(..)
                                                           , IRange(..), ItemID(..)
                                                           , NETagger(..)
                                                           , TextMatchedEntityType(..)
                                                           , PreNE(..)
                                                           , UIDCite(..)
                                                           , entityName
                                                           , entityPreNE
                                                           , getRangeFromEntityMention
                                                           )
import           WordNet.Type.Lexicographer                (LexicographerFile)
--
import           SRL.Analyze.Sense                         (getVerbSenses)
import           SRL.Analyze.Type
import           SRL.Analyze.UKB                           (runUKB)


tokenToTagPos ::
     IntMap CompanyInfo
  -> (Int,(Int,[Token]))
  -> Maybe (EntityMention Text)
tokenToTagPos _cmap (i,(cid,tks)) = do
  ft <- tks ^? _head
  lt <- tks ^? _last
  let b = ft ^. token_tok_idx_range . _1
      e = lt ^. token_tok_idx_range . _2
      txts = tks ^.. traverse . token_text
      ctyp = PublicCompany -- for the time being
  pure $
    Self
      (EntityMentionUID i)
      (IRange b e, V.fromList txts, OnlyTextMatched (CID cid) ctyp)


adjustWikiRange :: (Int,Int) -> (Int,Int)
adjustWikiRange (a,b) = (a,b-1)


linkedMentionToTagPos ::
     EntityMention Text
  -> TagPos TokIdx (EntityMention Text)
linkedMentionToTagPos linked_mention =
  let IRange b e = (_info linked_mention)^._1
  in TagPos (TokIdx b, TokIdx e,linked_mention)


mkWikiList :: IntMap CompanyInfo -> SentStructure -> [((Int, Int), Text)]
mkWikiList cmap sstr =
  let tagposs = sstr ^.. ss_tagged_full . traverse . to (fmap fst)
      wikiel  = lefts $ map (\(TagPos (i,j,e)) -> first (unTokIdx i,unTokIdx j,) e) tagposs
      wikilst = mapMaybe  (\(i,j,w) -> ((i,j-1),) <$> getNEFunc w) wikiel
      getNEFunc e =
        case (_info e)^._3 of
          UnresolvedUID x    -> Just (entityName (_info e) <> "(" <> T.pack (show x) <> ")" )
          AmbiguousUID (_,x) -> Just (entityName (_info e) <> "(" <> T.pack (show x)<> ")" )
          Resolved (i,c)     -> Just (entityName (_info e) <> "(" <> T.pack (show c) <> "," <> T.pack (show i) <> ")")
          OnlyTextMatched i x  -> Just (entityName (_info e) <> "(" <> T.pack (show x) <> "," <> T.pack (show i) <> maybe "" (\c -> "," <> c^.ticker) (IM.lookup (_citemID i) cmap)  <> ")" )
          UnresolvedClass _  -> Nothing
  in wikilst


nerDocument ::
     NETagger
  -> DocAnalysisInput
  -> [TagPos TokIdx (Either (EntityMention Text) (Char, Maybe Text))]
nerDocument netagger docinput@(DocAnalysisInput _ _ _ _ _ _ mtmxs) =
  let linked_mentions_resolved = unNETagger netagger (docinput^.dainput_sents)
      lnk_mntns_tagpos = map linkedMentionToTagPos linked_mentions_resolved
      mkidx = zipWith (\i x -> fmap (i,) x) (cycle ['a'..'z'])
  in maybe (map (fmap Left) lnk_mntns_tagpos) (mergeTagPos lnk_mntns_tagpos . mkidx) mtmxs


-- | Finding the structure of the sentence and formatting it.
--
docStructure ::
     AnalysisData
  -> DocAnalysisInput
  -> IO DocStructure
docStructure adata
  -- (AnalysisData sdata netagger (CompanyMap forest companyMap))
  docinput@(DocAnalysisInput sents sentidxs sentitems _ mptrs _ mtmxs) = do
  let lmass = sents ^.. traverse . sentenceLemma . to (map Lemma)
      -- need to revive
      -- mergedtags = nerDocument sdata netagger docinput
      mtokenss = sents ^.. traverse . sentenceToken
      linked_mentions_resolved =
        adata^.analysis_NETagger^.to unNETagger $ docinput^.dainput_sents
      CompanyMap forest companyMap = adata ^. analysis_CompanyMap
      entitiesByNER =
        map (evalState $ runExceptT $ many $ pTreeAdvGBy (\t -> (== (t ^. token_text))) forest) $
          map catMaybes mtokenss
      ne = concat $ rights entitiesByNER
      -- TODO: remove this hard-coded index (10001).
      tne = mapMaybe (tokenToTagPos companyMap) (zip [10001..] ne)
      tnerange = map getRangeFromEntityMention tne
      lnk_mntns1 = tne
      lnk_mntns2 = filter (\mntn -> not $ elemIsStrictlyInsideR (getRangeFromEntityMention mntn) tnerange) linked_mentions_resolved
      lnk_mntns_tagpos = map linkedMentionToTagPos (lnk_mntns1 ++ lnk_mntns2)
      mkidx = zipWith (\i x -> fmap (i,) x) (cycle ['a'..'z'])
  synsetss <- runUKB (adata^.analysis_SRLData.srldata_wordnet)(sents,mptrs)
  let mergedtags = maybe (map (fmap Left) lnk_mntns_tagpos) (mergeTagPos lnk_mntns_tagpos . mkidx) mtmxs
  let sentStructures =
        map (sentStructure (adata^.analysis_SRLData) mergedtags) $
          zip5 ([1..] :: [Int]) sentidxs lmass mptrs synsetss
  return (DocStructure mtokenss sentitems mergedtags sentStructures)


tagToMark ::
     Either (EntityMention Text) (Char,Maybe Text) -> Maybe MarkType
tagToMark (Right _) = Just MarkTime -- time is special
tagToMark (Left x)  = case entityPreNE x of
                        Resolved (_,c) ->
                          if | c == orgClass    -> Just (MarkEntity N.Org)
                             | c == personClass -> Just (MarkEntity N.Person)
                             | c == brandClass  -> Just (MarkEntity N.Other)
                             | otherwise        -> Nothing
                        UnresolvedUID c ->
                          if c `elem` [N.Org,N.Person]
                          then Just (MarkEntity c)
                          else Nothing
                        AmbiguousUID (_,c) ->
                          if c `elem` [N.Org,N.Person]
                          then Just (MarkEntity c)
                          else Nothing
                        OnlyTextMatched _ c ->
                          if | c == PublicCompany  -> Just (MarkEntity N.Org)
                             | c == PrivateCompany -> Just (MarkEntity N.Org)
                             | otherwise           -> Nothing
                        _ -> Nothing


adjustTokenIndexForSentence ::
     Maybe SentenceIndex
  -> [TagPos TokIdx (Either (EntityMention Text) (Char, Maybe Text))]
  -> [TagPos TokIdx (Either (EntityMention Text) (Char, Maybe Text), MarkType)]
adjustTokenIndexForSentence midx tagged
  = fromMaybe [] $ do
      (b0,e0) <- (^.sent_tokenRange) <$> midx
      return $ flip mapMaybe tagged $ \(TagPos (TokIdx b,TokIdx e,t)) -> do
        t' <- tagToMark t
        if b0 <= b && e <= e0
          then return (TagPos (TokIdx (b-b0),TokIdx (e-b0),(t,t')))
          else Nothing

sentStructure ::
     SRLData
  -> [TagPos TokIdx (Either (EntityMention Text) (Char,Maybe Text))]
  -> (Int,Maybe SentenceIndex,[Lemma],Maybe PennTree,[(Int,LexicographerFile)])
  -> Maybe SentStructure
sentStructure sdata taglst (i,midx,lmas,mptr,synsets) =
  flip fmap mptr $ \ptr ->
    let taglst' = adjustTokenIndexForSentence midx taglst
        taglstMarkOnly = map (fmap snd) taglst'
        lmatkns = (zip [0..] . zip lmas . map (^._2) . toList) ptr
        lemmamap = (mkLemmaMap . map unLemma) lmas
        pre = mkPreAnalysis lmatkns ptr taglstMarkOnly synsets
        vps = verbPropertyFromPennTree lemmamap (pre^.pennTree)
        x'tr = syntacticAnalysis pre
        verbStructures = map (verbStructure sdata pre) vps
    in SentStructure i ptr vps x'tr taglst' pre verbStructures


verbStructure ::
     SRLData
  -> PreAnalysis '[Lemma]
  -> VerbProperty (Zipper '[Lemma])
  -> VerbStructure
verbStructure sdata tagged vp =
  let i = vp^.vp_index
      l = vp^.vp_lemma
      ls = (map (^._2._1) . filter (\(j,_) -> j >= i)) (tagged^.lemmaList)
      (senses,rmtoppatts) = getVerbSenses sdata (l,ls)
  in VerbStructure vp senses rmtoppatts
