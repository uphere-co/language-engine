{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}

module SRL.Analyze.Match where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                (guard,join)
import           Data.Foldable
import           Data.Function                (on)
import qualified Data.HashMap.Strict    as HM
import           Data.List                    (find,groupBy,sortBy)
import           Data.Maybe                   (catMaybes,fromMaybe,listToMaybe,mapMaybe,maybeToList)
import qualified Data.Text              as T
import           Data.Text                    (Text)
--
import           Data.Bitree                  (getNodes,getRoot)
import           Data.BitreeZipper            (current,mkBitreeZipper,root)
import           Lexicon.Mapping.Causation    (causeDualMap,cm_baseFrame,cm_causativeFrame
                                              ,cm_externalAgent,cm_extraMapping)
import           Lexicon.Type
import           NLP.Syntax.Clause            (cpRange,findPAWS,resolveDP)
import           NLP.Syntax.Type
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty      (Voice(..))
--
import           SRL.Analyze.Parameter        (roleMatchWeightFactor)
import           SRL.Analyze.Type             (MGVertex(..),MGEdge(..),MeaningGraph(..)
                                              ,DocStructure
                                              ,SentStructure
                                              ,VerbStructure
                                              ,ds_sentStructures
                                              ,ss_clausetr,ss_mcpstr,ss_verbStructures
                                              ,vs_roleTopPatts,vs_vp
                                              ,mv_range,mv_id
                                              )

--
import Debug.Trace


allPAWSTriplesFromDocStructure
  :: DocStructure
  -> [[(Maybe [Bitree (Range, CP '[Lemma]) (Range, CP '[Lemma])]
       ,VerbStructure
       ,PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag)))]]
allPAWSTriplesFromDocStructure dstr = do
  msstr <- dstr^.ds_sentStructures
  sstr <- maybeToList msstr
  return (mkPAWSTriples sstr)


mkPAWSTriples :: SentStructure
              -> [(Maybe [Bitree (Range, CP '[Lemma]) (Range, CP '[Lemma])]
                  ,VerbStructure
                  ,PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag)))]
mkPAWSTriples sstr = do
  let clausetr = sstr^.ss_clausetr
      mcpstr = sstr^.ss_mcpstr
  vstr <- sstr ^.ss_verbStructures
  let vp = vstr^.vs_vp
  paws <- maybeToList (findPAWS clausetr vp mcpstr)
  return (mcpstr,vstr,paws)



pbArgForGArg :: GArg -> ArgPattern p GRel -> Maybe (Text,GRel)
pbArgForGArg garg patt = check patt_arg0 "arg0" <|>
                         check patt_arg1 "arg1" <|>
                         check patt_arg2 "arg2" <|>
                         check patt_arg3 "arg3" <|>
                         check patt_arg4 "arg4"
  where check l label = do a <- patt^.l
                           garg' <- findGArg a
                           if garg==garg' then Just (label,a) else Nothing


pbArgForPP :: ArgPattern p GRel -> [(Text,Text)]
pbArgForPP patt = catMaybes [ check patt_arg0 "arg0"
                            , check patt_arg1 "arg1"
                            , check patt_arg2 "arg2"
                            , check patt_arg3 "arg3"
                            , check patt_arg4 "arg4"
                            ]
  where check l label = do a <- patt^.l
                           case a of
                             GR_PP mprep -> (label,) <$> mprep
                             _           -> Nothing


matchSubject :: [(PBArg,FNFrameElement)]
             -> Zipper '[Lemma]
             -> ArgPattern p GRel
             -> Maybe (FNFrameElement, Zipper '[Lemma])
matchSubject rolemap dp patt = do
  (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
  (,dp) <$> lookup p rolemap


matchObjects :: [(PBArg,FNFrameElement)]
             -> VerbP '[Lemma]
             -> ArgPattern p GRel
             -> [(FNFrameElement, Zipper '[Lemma])]
matchObjects rolemap verbp patt = do
  (garg,obj') <- zip [GA1,GA2] (verbp^.complement)
  guard ((not.null) obj')
  Right obj <- [last obj']
  ctag <- case getRoot (current obj) of
            Left (_,node) -> [chunkTag node]
            _             -> []
  (p,a) <- maybeToList (pbArgForGArg garg patt)
  case ctag of
    NP   -> guard (a == GR_NP   (Just garg))
    S    -> guard (a == GR_SBAR (Just garg))
    SBAR -> guard (a == GR_SBAR (Just garg))
    _    -> []
  fe <- maybeToList (lookup p rolemap)
  return (fe,obj)


matchPP :: PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag))
        -> Text
        -> Maybe (Zipper '[Lemma])
matchPP paws prep = do
    Left (rng,_) <- find ppcheck (paws^.pa_candidate_args)
    tr <- current . root <$> paws^.pa_CP.maximalProjection
    find (\z -> case getRoot (current z) of Left (rng',_) -> rng' == rng; _ -> False) $ getNodes (mkBitreeZipper [] tr)
  where
    ppcheck (Left (_,S_PP prep')) = prep == prep'
    ppcheck _                     = False


matchPrepArgs :: [(PBArg,FNFrameElement)]
              -> PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag))
              -> ArgPattern p GRel
              -> [(FNFrameElement, Zipper '[Lemma])]
matchPrepArgs rolemap paws patt = do
  (p,prep) <- pbArgForPP patt
  z <- maybeToList (matchPP paws prep)
  (,z) <$> maybeToList (lookup p rolemap)


matchAgentForPassive :: [(PBArg,FNFrameElement)]
                     -> PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag))
                     -> ArgPattern p GRel
                     -> Maybe (FNFrameElement, Zipper '[Lemma])
matchAgentForPassive rolemap paws patt = do
    (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
    z <- matchPP paws "by"
    (,z) <$> lookup p rolemap



matchThemeForPassive :: [(PBArg,FNFrameElement)]
                     -> Zipper '[Lemma]
                     -> ArgPattern p GRel
                     -> Maybe (FNFrameElement, Zipper '[Lemma])
matchThemeForPassive rolemap dp patt = do
  (p,GR_NP (Just GA1)) <- pbArgForGArg GA1 patt
  (,dp) <$> lookup p rolemap


matchSO :: [(PBArg,FNFrameElement)]
        -> (Zipper '[Lemma], VerbP '[Lemma], PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag)))
        -> (ArgPattern p GRel, Int)
        -> ((ArgPattern p GRel, Int), [(FNFrameElement, Zipper '[Lemma])])
matchSO rolemap (dp,verbp,paws) (patt,num) =
  case verbp^.headX.vp_voice of
    Active -> ((patt,num), maybeToList (matchSubject rolemap dp patt) ++ matchObjects rolemap verbp patt ++ matchPrepArgs rolemap paws patt )
    Passive -> ((patt,num),catMaybes [matchAgentForPassive rolemap paws patt,matchThemeForPassive rolemap dp patt] ++ matchPrepArgs rolemap paws patt)



extendRoleMapForDual frame rolemap = fromMaybe (frame,rolemap) $ do
  dualmap <- lookup frame $ map (\c -> (c^.cm_baseFrame,c)) causeDualMap
  let frame' = dualmap^.cm_causativeFrame
      rolemap' = filter (\(k,v) -> k /= "frame" && k /= "arg0") rolemap
      rolemap'' =  map f rolemap'
        where f (k,v) = maybe (k,v) (k,) (lookup v (dualmap^.cm_extraMapping))
      rolemap''' = ("arg0",dualmap^.cm_externalAgent) : rolemap''
  return (frame',rolemap''')



numMatchedRoles = lengthOf (_2.folded)


matchRoles :: _ -> _ -> _ -> _ -> _ -> Maybe ((ArgPattern () GRel, Int),_)
matchRoles verbp paws rolemap toppattstats dp =
    (listToMaybe . sortBy cmpstat . head . groupBy eq . sortBy (flip compare `on` numMatchedRoles)) matched
  where
    matched = map (matchSO rolemap (dp,verbp,paws)) toppattstats
    cmpstat  = flip compare `on` (^._1._2)
    eq       = (==) `on` lengthOf (_2.folded)



-- matchFrameRolesForCauseDual
matchFrameRolesForCauseDual verbp paws toppatts mDP causetype (frame1,rolemap1) = 
  let (frame2,rolemap2) = if causetype == LVDual
                          then extendRoleMapForDual frame1 rolemap1
                          else (frame1,rolemap1)
      mselected1 = join (matchRoles verbp paws rolemap1 toppatts <$> mDP)
      mselected2 = join (matchRoles verbp paws rolemap2 toppatts <$> mDP)
  in case (mselected1,mselected2) of
       (Nothing,Nothing) -> (frame1,Nothing)
       (Just _ ,Nothing) -> (frame1,mselected1)
       (Nothing,Just _ ) -> (frame2,mselected2)
       (Just s1,Just s2) -> 
         case (compare `on` numMatchedRoles) s1 s2 of
           GT -> (frame1,mselected1)
           LT -> (frame2,mselected2)
           EQ -> (frame1,mselected1)   -- choose intransitive because transitive should
                                       -- have one more argument in general.



matchFrameRolesAll verbp paws mDP rmtoppatts = do
  (rm,toppatts) <- rmtoppatts
  let rolemap1 = rm^._1._2
      stat = rm^._2
  frame1 <- maybeToList (lookup "frame" rolemap1)
  causetype <- (\x -> if x == "dual" then LVDual else LVSingle) <$> maybeToList (lookup "cause" rolemap1)
  return (matchFrameRolesForCauseDual verbp paws toppatts mDP causetype (frame1,rolemap1),stat)


  
matchFrame :: (Maybe [Bitree (Range, CP '[Lemma]) (Range, CP '[Lemma])]
              ,VerbStructure
              ,PredArgWorkspace '[Lemma] (Either (Range,STag) (Int,POSTag)))
           -> Maybe (Range,VerbProperty (Zipper '[Lemma]),FNFrameElement
                    ,Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, Zipper '[Lemma])]))
matchFrame (mcpstr,vstr,paws) = do
  let cp = paws^.pa_CP
      verbp = cp^.complement.complement
      mDP = case cp^.complement.specifier of
              []  -> Nothing
              dps -> case last dps of
                       Left _ -> Nothing
                       Right z -> Just z
      vprop = vstr^.vs_vp
  rng <- cpRange cp
  let frmsels = matchFrameRolesAll verbp paws mDP (vstr^.vs_roleTopPatts)
  let total=  sum (frmsels^..traverse._2)
  ((frame,mselected),_) <- listToMaybe (sortBy (flip compare `on` scoreSelectedFrame total) frmsels)

  trace (show (map (\x@((fr,msel),n) -> (fr,n,scoreSelectedFrame total x)) frmsels)) $
    return (rng,vprop,frame,mselected)


scoreSelectedFrame total ((frame,mselected),n) =
  let mn = maybe 0 fromIntegral (mselected^?_Just.to numMatchedRoles)
  in mn * (fromIntegral n) / (fromIntegral total) * roleMatchWeightFactor + (mn*(fromIntegral total))


meaningGraph :: SentStructure -> MeaningGraph
meaningGraph sstr =
  let pawstriples = mkPAWSTriples sstr
      matched =  mapMaybe matchFrame pawstriples
      gettokens = T.intercalate " " . map (tokenWord.snd) . toList
      --
      preds = flip map matched $ \(rng,vprop,frame,_mselected) -> \i ->
                MGPredicate i rng frame
                            (vprop^.vp_lemma.to unLemma,vprop^.vp_tense,vprop^.vp_aspect,vprop^.vp_voice
                            ,vprop^?vp_auxiliary._Just._2._2.to unLemma
                            )
      ipreds = zipWith ($) preds [1..]
      --
      entities0 = do (_,_,_,mselected) <- matched
                     (_,felst) <- maybeToList mselected
                     (_fe,z) <- felst
                     let x = current z
                         rng = getRange x
                         txt = gettokens x
                     return (rng,txt)
      filterFrame = filter (\(rng,_) -> not (any (\p -> p^.mv_range == rng) ipreds))
      --
      entities = map (\(rng,txt) i -> MGEntity i rng txt)
               . filterFrame
               . map head
               . groupBy ((==) `on` (^._1))
               . sortBy (compare `on` (^._1))
               $ entities0
      vertices = ipreds ++ zipWith ($) entities (enumFrom (length ipreds+1))
      --
      rngidxmap = HM.fromList [(v^.mv_range,v^.mv_id) | v <- vertices ]
      edges = do (rng,_,_,mselected) <- matched
                 i <- maybeToList (HM.lookup rng rngidxmap)
                 (_,felst) <- maybeToList mselected
                 (fe,z) <- felst
                 let rng' = getRange (current z)
                 i' <- maybeToList (HM.lookup rng' rngidxmap)
                 return (MGEdge fe i i')
  in MeaningGraph vertices edges
