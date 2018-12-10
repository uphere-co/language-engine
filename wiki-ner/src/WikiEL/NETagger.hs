{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module WikiEL.NETagger
  ( dummyNETagger
  , newNETagger
  ) where

import           Control.Lens         ( (^.), _1, _3, to )
import           Data.Function        ( on )
import qualified Data.Set as S
import           Data.Text            ( Text )
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.List            ( foldl', maximumBy, sortBy )
import           Data.HashMap.Strict  ( HashMap )
import qualified Data.HashMap.Strict as HM
import           Data.Maybe           ( mapMaybe )
------ other language-engine
import           NLP.Type.NamedEntity ( NamedEntityClass(..) )
------ wiki-ner
import           WikiEL               ( extractFilteredEntityMentions )
import           WikiEL.Type          ( PreNE(..), UIDCite(..)
                                      , beg, end
                                      )
import           WikiEL.Run           ( runEL, classFilesG, reprFileG )
import qualified WikiEL.EntityLinking    as EL
import qualified WikiEL.ETL.LoadData     as LD
import           WikiEL.Type          ( NameUIDTable, NETagger(..), WikiuidNETag )
import qualified WikiEL.Type             as WT
import           WikiEL.Type.FileFormat ( EntityReprRow(..) )
import           WikiEL.Type.Wikidata   ( ItemID )
import qualified WikiEL.Type.Wikidata    as WD
import qualified WikiEL.WikiEntityClass  as WEC
import qualified WikiEL.WikiEntityTagger as WET


-- | Dummy NETagger if one wants to bypass NETagger
dummyNETagger :: NETagger
dummyNETagger = NETagger (const [])

data NEData = NEData { _nedataReprs     :: [EntityReprRow]
                     , _nedataWikiTable :: NameUIDTable
                     , _nedataWikiMap   :: HashMap ItemID [Text]
                     , _nedataUIDNETags :: WikiuidNETag
                     }

-- | Load table informations
loadNEData :: FilePath -> IO NEData
loadNEData dataDir = do
  reprs <- LD.loadEntityReprs (reprFileG dataDir)
  let wikiTable = WET.buildEntityTable reprs
      wikiMap = foldl' f HM.empty reprs
        where f !acc (EntityReprRow (WD.QID i) (WD.ItemRepr t)) = HM.insertWith (++) (WD.QID i) [t] acc
              f _ _ = error "f in newNETagger"
  uidNEtags <- WEC.loadFiles (classFilesG dataDir)
  pure $ NEData reprs wikiTable wikiMap uidNEtags


showNEDataStatus :: NEData -> IO ()
showNEDataStatus (NEData reprs wikiTable wikiMap uidNEtags) = do
  -- NOTE: for test
  putStrLn $ "length of reprs = " ++ show (length reprs)
  putStrLn $ "wikiTable: length(uid)   = " ++ (wikiTable ^. WT.uids . to V.length . to show)
  putStrLn $ "wikiTable: length(names) = " ++ (wikiTable ^. WT.names . to V.length . to show)
  putStrLn $ "wikiMap: size = " ++ show (HM.size wikiMap)
  putStrLn $ "length of uidNEtags = " ++ show (S.size (uidNEtags ^. WT.set))


-- | Create a new NETagger object.
newNETagger :: FilePath -> IO NETagger
newNETagger dataDir = do
  nedata@(NEData _reprs wikiTable wikiMap uidNEtags) <- loadNEData dataDir
  showNEDataStatus nedata
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
  let netagger = NETagger (runEL tagger (map disambiguator))
  pure netagger
