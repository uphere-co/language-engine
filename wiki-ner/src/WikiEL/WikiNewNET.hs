{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module WikiEL.WikiNewNET where

import           Control.Lens                  ((^.),_1,_3,to)
import           Data.Function                 (on)
import qualified Data.Set as S
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Data.List                     (foldl',maximumBy,sortBy)
import qualified Data.HashMap.Strict    as HM
import           Data.Maybe                    (mapMaybe)
--
import           NLP.Type.CoreNLP              (Sentence)
import           NLP.Type.NamedEntity          (NamedEntityClass(..))
--
import           WikiEL.Type                   (EntityMention,PreNE(..),UIDCite(..),beg,end)
import           WikiEL.Run                    (runEL,classFilesG,reprFileG)
import           WikiEL                        (extractFilteredEntityMentions)
import qualified WikiEL.WikiEntityClass  as WEC
import qualified WikiEL.WikiEntityTagger as WET
import qualified WikiEL.EntityLinking    as EL
import qualified WikiEL.ETL.LoadData     as LD
import qualified WikiEL.Type             as WT
import qualified WikiEL.Type.FileFormat  as FF
import qualified WikiEL.Type.Wikidata    as WD


newNETagger :: FilePath -> IO ([Sentence] -> [EntityMention T.Text])
newNETagger dataDir = do
  reprs <- LD.loadEntityReprs (reprFileG dataDir)
  -- NOTE: for test
  putStrLn $ "length of reprs = " ++ show (length reprs)
  let wikiTable = WET.buildEntityTable reprs
  -- NOTE: for test
  putStrLn $ "wikiTable: length(uid)   = " ++ (wikiTable ^. WT.uids . to V.length . to show)
  putStrLn $ "wikiTable: length(names) = " ++ (wikiTable ^. WT.names . to V.length . to show)
  let wikiMap = foldl' f HM.empty reprs
        where f !acc (FF.EntityReprRow (WD.QID i) (WD.ItemRepr t)) = HM.insertWith (++) (WD.QID i) [t] acc
              f _ _ = error "f in newNETagger"
  -- NOTE: for test
  putStrLn $ "wikiMap: size = " ++ show (HM.size wikiMap)
  uidNEtags <- WEC.loadFiles (classFilesG dataDir)
  -- NOTE: for test
  putStrLn $ "length of uidNEtags = " ++ show (S.size (uidNEtags ^. WT.set))
  let tagger = extractFilteredEntityMentions wikiTable uidNEtags
      disambiguatorWorker x (ys,t) =
        let lst = sortBy (flip compare `on` (length.snd)) .  mapMaybe (\y-> (y,) <$> HM.lookup y wikiMap) $ ys
       in case lst of
            [] -> x
            (r:_) -> let (i1,i2,_) = _info x
                         u = WEC.guessItemClass2 uidNEtags t
                         resolved = r^._1
                     in x { _info = (i1,i2,Resolved (resolved,u resolved)) }

  let disambiguator x =
        case ((^._3) . _info) x of
          AmbiguousUID (ys,t) -> disambiguatorWorker x (ys,t)
          UnresolvedUID t ->
            if t == Org || t == Person
            then
              let name0 = EL.entityName (_info x)
                  name = (T.replace "," "" . T.replace "." "") name0   -- try once more
                  tags' = WET.wikiAnnotator wikiTable (T.words name)
                  tags'' = filter (\(r,_)->(r^.end)-(r^.beg)>1) tags'
              in case tags'' of
                   [] -> x
                   _  -> let rids = (V.toList . snd . maximumBy (flip compare `on` (\(r,_) -> (r^.end) - (r^.beg)))) tags''
                         in disambiguatorWorker x (rids,t)
            else x
          _ -> x
  return (runEL tagger (map disambiguator))
