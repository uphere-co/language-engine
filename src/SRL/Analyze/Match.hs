{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module SRL.Analyze.Match where

import           Control.Applicative
import           Control.Lens
import           Control.Monad           (guard)
import           Data.Function           (on)
import           Data.List               (find,groupBy,sortBy)
import           Data.Maybe              (catMaybes,listToMaybe,maybeToList)
--
import           Data.Bitree             (getNodes,getRoot)
import           Data.BitreeZipper       (current,mkBitreeZipper)
import           Data.BitreeZipper.Util  (root)
import           Lexicon.Type
import           NLP.Syntax.Clause       (findPAWS,resolveDP)
import           NLP.Syntax.Type
import           NLP.Type.PennTreebankII
--
import           SRL.Analyze.Type        (ds_sentStructures
                                         ,ss_clausetr,ss_mcpstr,ss_verbStructures
                                         ,vs_mrmmtoppatts,vs_vp)

matchFrame mcpstr vstr paws = do
  let cp = paws^.pa_CP
      verbp = cp^.cp_TP.tp_VP
      mrmmtoppatts = vstr^.vs_mrmmtoppatts
      mdp_resolved = resolveDP mcpstr cp
  (rm,mtoppatts) <- mrmmtoppatts
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
  return (frame,selected)



mkPAWSTriples dstr = do
  msstr <- dstr^.ds_sentStructures
  sstr <- maybeToList msstr
  let clausetr = sstr^.ss_clausetr
      mcpstr = sstr^.ss_mcpstr
  vstr <- sstr ^.ss_verbStructures
  let vp = vstr^.vs_vp
  paws <- maybeToList (findPAWS clausetr vp)
  return (mcpstr,vstr,paws)


matchSO rolemap (dp,verbp,paws) (patt,num) =
  case verbp^.vp_verbProperty.vp_voice of
    Active -> ((patt,num), maybeToList (matchSubject rolemap dp patt) ++ matchObjects rolemap verbp patt ++ matchPrepArgs rolemap paws patt )
    Passive -> ((patt,num),catMaybes [matchAgentForPassive rolemap paws patt,matchThemeForPassive rolemap dp patt] ++ matchPrepArgs rolemap paws patt)


matchPrepArgs rolemap paws patt = do
  (p,prep) <- pbArgForPP patt
  z <- maybeToList (matchPP paws prep)
  (,z) <$> maybeToList (lookup p rolemap)


matchPP paws prep = do
    Left (rng,_) <- find ppcheck (paws^.pa_candidate_args)
    tr <- current . root <$> paws^.pa_CP.cp_maximal_projection
    find (\z -> case getRoot (current z) of Left (rng',_) -> rng' == rng; _ -> False) $ getNodes (mkBitreeZipper [] tr)
  where
    ppcheck (Left (_,S_PP prep')) = prep == prep'
    ppcheck _                     = False



matchSubject rolemap dp patt = do
  (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
  (,dp) <$> lookup p rolemap


matchAgentForPassive rolemap paws patt = do
    (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
    z <- matchPP paws "by"
    (,z) <$> lookup p rolemap




matchThemeForPassive rolemap dp patt = do
  (p,GR_NP (Just GA1)) <- pbArgForGArg GA1 patt
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

