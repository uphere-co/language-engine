{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module SRL.Analyze.Match where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                (guard)
import           Data.Foldable
import           Data.Function                (on)
import qualified Data.HashMap.Strict    as HM
import           Data.List                    (find,groupBy,sortBy)
import           Data.Maybe                   (catMaybes,listToMaybe,mapMaybe,maybeToList)
import qualified Data.Text              as T
--
import           Data.Bitree                  (getNodes,getRoot)
import           Data.BitreeZipper            (current,mkBitreeZipper)
import           Data.BitreeZipper.Util       (root)
import           Lexicon.Type
import           NLP.Syntax.Clause            (cpRange,findPAWS,resolveDP)
import           NLP.Syntax.Type
import           NLP.Type.PennTreebankII
--
import           SRL.Analyze.Type             (MGVertex(..),MGEdge(..),ds_sentStructures
                                              ,ss_clausetr,ss_mcpstr,ss_verbStructures
                                              ,vs_lma,vs_mrmmtoppatts,vs_vp
                                              ,mv_range,mv_id
                                              )


allPAWSTriplesFromDocStructure dstr = do
  msstr <- dstr^.ds_sentStructures
  sstr <- maybeToList msstr
  return (mkPAWSTriples sstr)


mkPAWSTriples sstr = do
  let clausetr = sstr^.ss_clausetr
      mcpstr = sstr^.ss_mcpstr
  vstr <- sstr ^.ss_verbStructures
  let vp = vstr^.vs_vp
  paws <- maybeToList (findPAWS clausetr vp)
  return (mcpstr,vstr,paws)


  

pbArgForGArg grel patt = check patt_arg0 "arg0" <|>
                         check patt_arg1 "arg1" <|>
                         check patt_arg2 "arg2" <|>
                         check patt_arg3 "arg3" <|>
                         check patt_arg4 "arg4"
  where check l label = do a <- patt^.l
                           grel' <- findGArg a
                           if grel==grel' then Just (label,a) else Nothing


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



matchSubject rolemap dp patt = do
  (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
  (,dp) <$> lookup p rolemap


matchObjects rolemap verbp patt = do
  (garg,obj) <- zip [GA1,GA2] (verbp^.vp_complements)
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



matchPP paws prep = do
    Left (rng,_) <- find ppcheck (paws^.pa_candidate_args)
    tr <- current . root <$> paws^.pa_CP.cp_maximal_projection
    find (\z -> case getRoot (current z) of Left (rng',_) -> rng' == rng; _ -> False) $ getNodes (mkBitreeZipper [] tr)
  where
    ppcheck (Left (_,S_PP prep')) = prep == prep'
    ppcheck _                     = False

matchPrepArgs rolemap paws patt = do
  (p,prep) <- pbArgForPP patt
  z <- maybeToList (matchPP paws prep)
  (,z) <$> maybeToList (lookup p rolemap)



matchAgentForPassive rolemap paws patt = do
    (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
    z <- matchPP paws "by"
    (,z) <$> lookup p rolemap




matchThemeForPassive rolemap dp patt = do
  (p,GR_NP (Just GA1)) <- pbArgForGArg GA1 patt
  (,dp) <$> lookup p rolemap



matchSO rolemap (dp,verbp,paws) (patt,num) =
  case verbp^.vp_verbProperty.vp_voice of
    Active -> ((patt,num), maybeToList (matchSubject rolemap dp patt) ++ matchObjects rolemap verbp patt ++ matchPrepArgs rolemap paws patt )
    Passive -> ((patt,num),catMaybes [matchAgentForPassive rolemap paws patt,matchThemeForPassive rolemap dp patt] ++ matchPrepArgs rolemap paws patt)



matchFrame (mcpstr,vstr,paws) = do
  let cp = paws^.pa_CP
      verbp = cp^.cp_TP.tp_VP
      mrmmtoppatts = vstr^.vs_mrmmtoppatts
      mdp_resolved = resolveDP mcpstr cp
      verb = vstr^.vs_lma
  (rm,mtoppatts) <- mrmmtoppatts
  rng <- cpRange cp
  let rolemap = rm^._2
  frame <- lookup "frame" rolemap
  let selected = do
        toppattstats <- mtoppatts
        dp_resolved <- mdp_resolved
        let matched = map (matchSO rolemap (dp_resolved,verbp,paws)) toppattstats
            cmpmatch = flip compare `on` lengthOf (_2.folded)
            cmpstat  = flip compare `on` (^._1._2)
            eq       = (==) `on` lengthOf (_2.folded)
        (listToMaybe . sortBy cmpstat . head . groupBy eq . sortBy cmpmatch) matched
  return (rng,verb,frame,selected)


meaningGraph sstr = do
  let pawstriples = mkPAWSTriples sstr
      matched =  mapMaybe matchFrame pawstriples
      gettokens = T.intercalate " " . map (tokenWord.snd) . toList

      preds = map (\(rng,verb,frame,mselected) -> (\i -> MGPredicate i rng frame verb)) matched
      ipreds = zipWith ($) preds [1..]
      entities0 = do (_,_,_,mselected) <- matched
                     (_,felst) <- maybeToList mselected
                     (fe,z) <- felst
                     let x = current z
                         rng = getRange x
                         txt = gettokens x
                     return (rng,txt)

      filterFrame = filter (\(rng,_) -> not (any (\p -> p^.mv_range == rng) ipreds))


      entities = map (\(rng,txt) i -> MGEntity i rng txt) 
               . filterFrame
               . map head
               . groupBy ((==) `on` (^._1))
               . sortBy (compare `on` (^._1))
               $ entities0
      vertices = ipreds ++ zipWith ($) entities (enumFrom (length ipreds+1))
      rngidxmap = HM.fromList [(v^.mv_range,v^.mv_id) | v <- vertices ]
      edges = do (rng,_,_,mselected) <- matched
                 i <- maybeToList (HM.lookup rng rngidxmap)
                 (_,felst) <- maybeToList mselected
                 (fe,z) <- felst
                 let rng' = getRange (current z)
                 i' <- maybeToList (HM.lookup rng' rngidxmap)
                 return (MGEdge fe i i')
  mapM_ print vertices
  mapM_ print edges
  {-
  flip mapM_ frames $ \(frame,mselected) ->
    flip traverse_ mselected $ \(_,felst) -> do
      let gettokens = T.intercalate " " . map (tokenWord.snd) . toList
    
          pairs = map (\(fe,z) -> let x = current z in (getRange x, gettokens x)) felst
      print (frame,pairs)
  -}
