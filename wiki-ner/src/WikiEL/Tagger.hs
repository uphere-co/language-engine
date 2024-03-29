{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module WikiEL.Tagger where

import           Control.Arrow        ( second )
import           Control.Lens         ( (^.), (^..), _1, _3, to )
import           Data.Function        ( on )
import qualified Data.List as L
import           Data.Maybe           ( catMaybes, fromMaybe )
import qualified Data.Set as S
import           Data.Text            ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import           Data.Vector          ( Vector )
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed as UV
import           Data.List            ( foldl', maximumBy, sortBy )
import           Data.HashMap.Strict  ( HashMap )
import qualified Data.HashMap.Strict as HM
import           Data.Maybe           ( mapMaybe )
import           System.FilePath      ( (</>) )
------ other language-engine
import           Graph.Internal.Hash  ( wordHash )
import           NLP.Type.NamedEntity ( NamedEntityClass(..)
                                      , NamedEntityFrag(..)
                                      , classify
                                      )
import           NLP.Type.PennTreebankII ( POSTag(..), TernaryLogic(..), isNoun )
import           NLP.Type.CoreNLP     ( Sentence
                                      , sentenceNER
                                      , sentenceToken
                                      , sentenceWord
                                      , token_pos
                                      )
------ wiki-ner
import WikiEL.Match ( greedyAnnotation )
import WikiEL.Parser (getEntityRepr,getItemID)
import WikiEL.Type ( EntityMention
                   , EntityMentionUID(..)
                   , EntityReprFile(..)
                   , EntityReprRow(..)
                   , EMInfo
                   , IRange(..)
                   , ItemClass(..)
                   , ItemID(..)
                   , ItemIDRow
                   , ItemIDFile(..)
                   , ItemRepr(..)
                   , NameUIDTable(..)
                   , NETagger(..)
                   , PreNE(..)
                   , RelativePosition(..)
                   , UIDCite(..)
                   , WikiuidNETag(..)
                   , type WordsHash
                   , beg
                   , end
                   , set
                   , uids
                   , names
                   , entityName
                   )
import WikiEL.Util ( relativePos, strictSlice, subVector, untilNoOverlap )


buildEntityTable :: [EntityReprRow] -> NameUIDTable
buildEntityTable entities = NameUIDTable uids names
  where
    nameOrdering (_lhsUID, lhsName) (_rhsUID, rhsName) = compare lhsName rhsName
    entitiesByName = V.modify (VA.sortBy nameOrdering) (V.fromList (map itemTuple entities))
    uids  = V.map fst entitiesByName
    names = V.map snd entitiesByName

wikiAnnotator:: NameUIDTable -> [Text] -> [(IRange, Vector ItemID)]
wikiAnnotator entities ws = matchedItems
  where
    tokens    = wordsHash ws
    matchedIdxs  = greedyAnnotation (_names entities) tokens
    matchedItems = map (second (V.map (V.unsafeIndex (_uids entities)))) matchedIdxs


-- old WikiEL.WikiEntityClass

extendedClasses :: [ItemClass]
extendedClasses = [brandClass,occupationClass,humanRuleClass,buildingClass]

allClasses :: [ItemClass]
allClasses      = [otherClass, orgClass, personClass, locationClass] ++ extendedClasses

{-|
  A list of constant values for Wikipedia entity classes
-}
otherClass,orgClass,personClass,locationClass,brandClass,occupationClass,humanRuleClass,buildingClass :: ItemClass
otherClass  = buildItemClass "Q35120"  "Other"-- maps to entity (Q35120), which means "anything"
orgClass    = buildItemClass "Q43229"  "Organization"
personClass = buildItemClass "Q215627" "Person"
locationClass   = buildItemClass "Q1496967"  "Location"-- territorial entity (Q1496967), instead of location (Q17334923)
brandClass      = buildItemClass "Q431289"   "Brand"
occupationClass = buildItemClass "Q12737077" "Occupation"
humanRuleClass  = buildItemClass "Q1151067"  "HumanRule"
buildingClass   = buildItemClass "Q41176"    "Building"

buildItemClass :: Text -> Text -> ItemClass
buildItemClass x name = ItemClass (getItemID x) name

wec_loadFiles :: [(ItemClass, ItemIDFile)] -> IO WikiuidNETag
wec_loadFiles pairs = do
  lists <- mapM loadTypedUIDs pairs
  return $ WikiuidNETag (S.fromList (mconcat lists))

{-|
  Since CoreNLP NER classes, NEClass, are more coarse, we need inclusion, or a sort of subclass, relationship
  between ItemClass and NEClass.
-}
mayCite :: NamedEntityClass -> ItemClass -> Bool
mayCite Org    c | c==orgClass      = True
mayCite Person c | c==personClass   = True
mayCite Loc    c | c==locationClass = True
mayCite Date   c | c==humanRuleClass= True
mayCite Time   c | c==humanRuleClass= True
mayCite Money  c | c==humanRuleClass= True
mayCite Other  _             = True
mayCite Misc   _             = True
mayCite _      _             = False

guessItemClass :: WikiuidNETag -> ItemID -> ItemClass
guessItemClass (WikiuidNETag tags) i = fromMaybe otherClass x
  where
    x = L.find (\y -> S.member (i,y) tags) allClasses

guessItemClass2 :: WikiuidNETag -> NamedEntityClass -> ItemID -> ItemClass
guessItemClass2 tags ne i | hasNETag tags (i,ne) = fromNEClass ne
                          | otherwise            = guessItemClass tags i
-- guessItemClass2 tags ne i = guessItemClass tags id


fromNEClass :: NamedEntityClass -> ItemClass
fromNEClass Org    = orgClass
fromNEClass Person = personClass
fromNEClass Loc    = locationClass
fromNEClass _      = otherClass




loadTypedUIDs :: (ItemClass , ItemIDFile) -> IO [(ItemID, ItemClass)]
loadTypedUIDs (tag, fileName) = do
  items <- loadItemIDs fileName
  let
    uids = map (\x -> (x, tag)) items
  return uids

hasNETag :: WikiuidNETag -> (ItemID, NamedEntityClass) -> Bool
hasNETag (WikiuidNETag tags) (i,stag) | stag /= Other = S.member (i,fromNEClass stag) tags
                                      | otherwise     = any (\x -> S.member (i,x) tags) extendedClasses

-- hasNETag (WikiuidNETag tags) (i,stag) = any (\x -> S.member (i,x) tags) extendedClasses

-- old WikiEL.ETL.LoadData

loadFile :: (a->FilePath) -> (Text->b) -> a -> IO [b]
loadFile p f file = do
  filetxt <- T.IO.readFile (p file)
  let
    rows = map f (T.lines filetxt)
  return rows


loadEntityReprs :: EntityReprFile -> IO [EntityReprRow]
loadEntityReprs = loadFile unEntityReprFile getEntityRepr

loadItemIDs :: ItemIDFile -> IO [ItemIDRow]
loadItemIDs = loadFile unItemIDFile getItemID

-- old WikiEL.EntityLinking

entityIRange :: EntityMention a -> IRange
entityIRange mention = range
  where (range, _, _) = _info mention

buildEntityMentions :: Vector w -> [(IRange, PreNE)] -> [EntityMention w]
buildEntityMentions text wikiNEs = zipWith Self uids mentions
  where
    uids     = map EntityMentionUID [1..]
    mentions = map (\(range,e) -> (range, subVector range text, e)) wikiNEs


tryEntityLink :: Eq a => EMInfo a -> EMInfo a -> Maybe (EMInfo a)
tryEntityLink (trange, twords, tNE) (srange, swords, sNE) =
  f (relativePos trange srange) (strictSlice swords twords) tNE sNE
  where
    g ttag (Resolved (_, s))       = s==ttag
    g ttag (UnresolvedUID stag)    = mayCite stag ttag
    g ttag (AmbiguousUID (_,stag)) = mayCite stag ttag
    g _ttag _                      = False

    f pos textMatch (Resolved (uid,ttag)) src | pos==LbeforeR && textMatch && g ttag src =
      Just (srange,swords, Resolved (uid,ttag))
    f _ _ _ _ = Nothing



entityLinking :: Eq a => [EntityMention a] -> EntityMention a -> EntityMention a
entityLinking targets source = foldr f source targets
  where
    f target src@(Self idx info)       =
      case tryEntityLink (_info target) info of
        Nothing   -> src
        Just linked -> Cite idx (_uid target) linked
    f _      src = src

{-
  entityLinkings tries to resolve unresolved or ambiguous entity mentions using :
    1. phrase inclusion
    2. entity type matching (see WikiEL.WikiEntityClass.mayCite for details)
-}
entityLinkings :: Eq a => [EntityMention a] -> [EntityMention a]
entityLinkings srcs = reverse (foldl' f [] srcs)
  where
    f targets src = entityLinking targets src : targets



-- old WikiEL.Run

runEL :: ([(Text,NamedEntityClass,POSTag)] -> [EntityMention Text])
      -> ([EntityMention Text] -> [EntityMention Text])
      -> [Sentence]
      -> [EntityMention Text]
runEL tagger entityResolve sents  =
  let wnps = prepareWNP sents
      linked_mentions = tagger wnps
  in entityResolve linked_mentions


classFilesG :: FilePath -> [(ItemClass,ItemIDFile)]
classFilesG dataDir =
  [ (personClass    , personItemFileG     dataDir)
  , (orgClass       , orgItemFileG        dataDir)
  , (brandClass     , brandItemFileG      dataDir)
  , (occupationClass, occupationItemFileG dataDir)
  , (locationClass  , locationItemFileG   dataDir)
  , (humanRuleClass , humanRuleItemFileG  dataDir)
  , (buildingClass  , buildingItemFileG   dataDir)
  ]

orgItemFileG,personItemFileG,brandItemFileG,locationItemFileG,occupationItemFileG,humanRuleItemFileG,buildingItemFileG :: FilePath-> ItemIDFile
orgItemFileG        dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.org")
personItemFileG     dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.person")
brandItemFileG      dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.brand")
locationItemFileG   dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.loc")
occupationItemFileG dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.occupation")
humanRuleItemFileG  dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.human_rule")
buildingItemFileG   dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.building")



reprFileG :: FilePath -> EntityReprFile
reprFileG dataDir =
  EntityReprFile (dataDir </> "wiki-ner/data/names")


prepareWNP :: [Sentence] -> [(Text,NamedEntityClass,POSTag)]
prepareWNP sents =
  let ws = catMaybes $ sents ^.. traverse . sentenceWord . traverse
      ns' = catMaybes $ sents ^.. traverse . sentenceNER . traverse
      ns = map (\x -> (fromMaybe (error (show x)) . classify) x) ns'
      ts = catMaybes $ sents ^.. traverse . sentenceToken . traverse
      ps = map (^. token_pos) ts
  in (zip3 ws ns ps)


-- old WikiEL.EntityMentionPruning

isEntityLinkableTag :: POSTag -> Bool
isEntityLinkableTag = f . n
  where
    n PRP = No
    n x   = isNoun x
    f Yes = True
    f _   = False

isEntityLinkable :: V.Vector POSTag -> IRange -> Bool
isEntityLinkable tags (IRange beg end) = V.any isEntityLinkableTag tokens
  where
    tokens = V.slice beg (end-beg) tags

{- |
  filterEMbyPOS requires that a text of an entity mention should contain a noun phrase.
-}
filterEMbyPOS :: V.Vector POSTag -> [EntityMention w] -> [EntityMention w]
filterEMbyPOS wholeTags = filter f
  where
    f mention = isEntityLinkable wholeTags (entityIRange mention)


-- old WikiEL.WikiNamedEntityTagger



wordsHash :: [Text] -> WordsHash
wordsHash = UV.fromList . map wordHash


nameWordsHash :: ItemRepr -> WordsHash
nameWordsHash (ItemRepr name) = wordsHash (T.words name)


itemTuple :: EntityReprRow -> (ItemID, WordsHash)
itemTuple (EntityReprRow uid name) = (uid, nameWordsHash name)





namedEntityAnnotator:: NameUIDTable -> [NamedEntityFrag] -> [(IRange, Vector ItemID)]
namedEntityAnnotator entities frags = reverse matchedItems
  where
    ws = map _fstr frags
    matchedItems = wikiAnnotator entities ws


partitonFrags:: [NamedEntityFrag] -> [(IRange, NamedEntityClass)]
partitonFrags frags = V.ifoldr f [] (V.fromList frags)
  where
    decL (IRange beg end) = IRange (beg-1) end
    toRange idx = IRange idx (idx+1)
    tagType = _ftype
    g idx frag = (toRange idx, tagType frag)
    f idx frag [] = [g idx frag]
    f idx frag accum@((range, tag):ss) | tagType frag == tag = (decL range, tag):ss
                                       | otherwise            = g idx frag : accum


getStanfordNEs :: [NamedEntityFrag] -> [(IRange, NamedEntityClass)]
getStanfordNEs = dropNonNE . partitonFrags
  where
    dropNonNE = filter (\x-> snd x /= Other)



resolveNEClass :: WikiuidNETag -> NamedEntityClass -> Vector ItemID -> PreNE
resolveNEClass ts stag xs = g matchedUIDs
  where
    u = guessItemClass2 ts stag
    f !accum uid | mayCite stag (u uid)  = uid:accum
                 | otherwise             = accum
    matchedUIDs = foldl' f [] xs
    g [uid] = Resolved     (uid, u uid)
    g []    = UnresolvedUID stag
    g uids  = AmbiguousUID (uids, stag)


resolveNEsImpl :: WikiuidNETag -> [(IRange,PreNE)] -> [(IRange, NamedEntityClass)] -> [(IRange, Vector ItemID)] -> [(IRange,PreNE)]
resolveNEsImpl _ts accum [] [] = accum
resolveNEsImpl ts accum ((lrange,ltag):ls) []  =
  resolveNEsImpl ts ((lrange, UnresolvedUID ltag) : accum) ls []
resolveNEsImpl ts accum [] ((rrange,rtags):rs) =
  resolveNEsImpl ts ((rrange, UnresolvedClass (V.toList rtags)) : accum) [] rs
resolveNEsImpl ts accum lhss@((lrange,ltag):ls) rhss@((rrange,rtags):rs) =
  --case mergeDecision (relativePos lrange rrange) of
  case relativePos lrange rrange of
    Coincide  -> resolveNEsImpl ts ((lrange, resolveNEClass ts ltag rtags):accum) ls rs
    LinR      -> resolveNEsImpl ts (keepR:accum) lsIter rs
    RinL      -> resolveNEsImpl ts (keepL:accum) ls rsIter
    LbeforeR  -> resolveNEsImpl ts (keepL:accum) ls rhss
    RbeforeL  -> resolveNEsImpl ts (keepR:accum) lhss rs
    RoverlapL -> resolveNEsImpl ts (keepR:accum) ls rsIter
    LoverlapR -> resolveNEsImpl ts (keepL:accum) lsIter rs
  where
    keepL = (lrange, UnresolvedUID ltag)
    keepR = (rrange, UnresolvedClass (V.toList rtags))
    lsIter = untilNoOverlap (relativePos rrange . fst) ls
    rsIter = untilNoOverlap (relativePos lrange . fst) rs



resolveNEs :: WikiuidNETag -> [(IRange, NamedEntityClass)] -> [(IRange, Vector ItemID)] -> [(IRange,PreNE)]
resolveNEs ts lhss rhss = map (second assumeCorrectAnnotation) xs
  where
    xs = reverse (resolveNEsImpl ts [] lhss rhss)
    assumeCorrectAnnotation (UnresolvedClass [itemID]) = Resolved (itemID, guessItemClass ts itemID)
    assumeCorrectAnnotation x = x



-- old WikiEL


extractEntityMentions :: NameUIDTable -> WikiuidNETag -> [(Text, NamedEntityClass)] -> [EntityMention Text]
extractEntityMentions wikiTable uidNEtags neTokens = linked_mentions
  where
    stanford_nefs  = map (uncurry NamedEntityFrag) neTokens
    named_entities = getStanfordNEs stanford_nefs
    wiki_entities  = namedEntityAnnotator wikiTable stanford_nefs
    wiki_named_entities = resolveNEs uidNEtags named_entities wiki_entities

    ws    = V.fromList (map fst neTokens)
    mentions = buildEntityMentions ws wiki_named_entities
    linked_mentions = entityLinkings mentions



extractFilteredEntityMentions :: NameUIDTable -> WikiuidNETag -> [(Text, NamedEntityClass, POSTag)] -> [EntityMention Text]
extractFilteredEntityMentions wikiTable uidNEtags tokens = filtered_mentions
  where
    neTokens = map (\(x,y,_)->(x,y)) tokens
    poss     = map (\(_,_,z)->z)     tokens
    all_linked_mentions = extractEntityMentions wikiTable uidNEtags neTokens
    filtered_mentions   = filterEMbyPOS (V.fromList poss) all_linked_mentions




-- old WikiEL.NETagger

data NEData = NEData { _nedataReprs     :: [EntityReprRow]
                     , _nedataWikiTable :: NameUIDTable
                     , _nedataWikiMap   :: HashMap ItemID [Text]
                     , _nedataUIDNETags :: WikiuidNETag
                     }

-- | Load table informations
loadNEData :: FilePath -> IO NEData
loadNEData dataDir = do
  reprs <- loadEntityReprs (reprFileG dataDir)
  let wikiTable = buildEntityTable reprs
      wikiMap = foldl' f HM.empty reprs
        where f !acc (EntityReprRow (QID i) (ItemRepr t)) = HM.insertWith (++) (QID i) [t] acc
              f _ _ = error "f in newNETagger"
  uidNEtags <- wec_loadFiles (classFilesG dataDir)
  pure $ NEData reprs wikiTable wikiMap uidNEtags


showNEDataStatus :: NEData -> IO ()
showNEDataStatus (NEData reprs wikiTable wikiMap uidNEtags) = do
  -- NOTE: for test
  putStrLn $ "length of reprs = " ++ show (length reprs)
  putStrLn $ "wikiTable: length(uid)   = " ++ (wikiTable ^. uids . to V.length . to show)
  putStrLn $ "wikiTable: length(names) = " ++ (wikiTable ^. names . to V.length . to show)
  putStrLn $ "wikiMap: size = " ++ show (HM.size wikiMap)
  putStrLn $ "length of uidNEtags = " ++ show (S.size (uidNEtags ^. set))








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
                         u = guessItemClass2 uidNEtags t
                         resolved = r^._1
                     in x { _info = (i1,i2,Resolved (resolved,u resolved)) }

  let disambiguator x =
        case ((^._3) . _info) x of
          AmbiguousUID (ys,t) -> disambiguatorWorker x (ys,t)
          UnresolvedUID t ->
            if t == Org || t == Person
            then
              let name0 = entityName (_info x)
                  name = (T.replace "," "" . T.replace "." "") name0   -- try once more
                  tags' = wikiAnnotator wikiTable (T.words name)
                  tags'' = filter (\(r,_)->(r^.end)-(r^.beg)>1) tags'
              in case tags'' of
                   [] -> x
                   _  -> let rids = (V.toList . snd . maximumBy (flip compare `on` (\(r,_) -> (r^.end) - (r^.beg)))) tags''
                         in disambiguatorWorker x (rids,t)
            else x
          _ -> x
  let netagger = NETagger (runEL tagger (map disambiguator))
  pure netagger
