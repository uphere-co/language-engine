{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WordNet.Query where

import           Control.Lens
import           Control.Monad              (join)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap         as IM
import           Data.Maybe                 (catMaybes,maybeToList)
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Text.Read
import           System.FilePath
--
import           WordNet.Format
import           WordNet.Parser.Common
import           WordNet.Parser.Data
import           WordNet.Parser.Index
import           WordNet.Type
import           WordNet.Type.POS


data WordNetDB = WNDB { _indexNounDB :: HM.HashMap Text IndexItem -- [(SenseNumber,SynsetOffset)]
                      , _indexVerbDB :: HM.HashMap Text IndexItem -- [(SenseNumber,SynsetOffset)]
                      , _indexAdjDB  :: HM.HashMap Text IndexItem  -- [(SenseNumber,SynsetOffset)]
                      , _indexAdvDB  :: HM.HashMap Text IndexItem -- [(SenseNumber,SynsetOffset)]
                      , _dataNounDB  :: IM.IntMap DataItem -- ([LexItem],Text)
                      , _dataVerbDB  :: IM.IntMap DataItem -- ([LexItem],Text)
                      , _dataAdjDB   :: IM.IntMap DataItem -- ([LexItem],Text)
                      , _dataAdvDB   :: IM.IntMap DataItem -- ([LexItem],Text)
                      -- , _senseIdxDB  :: HM.HashMap (Text,Int) Int
                      }

makeLenses ''WordNetDB                 


createWordNetDB :: ([IndexItem],[IndexItem],[IndexItem],[IndexItem])
                -> ([DataItem],[DataItem],[DataItem],[DataItem])
                -- -> [SenseItem]
                -> WordNetDB 
createWordNetDB ilsts dlsts {- slists -} =
  WNDB (createLemmaMap (ilsts^._1))
       (createLemmaMap (ilsts^._2))
       (createLemmaMap (ilsts^._3))
       (createLemmaMap (ilsts^._4))
       (createSynsetMap (dlsts^._1))
       (createSynsetMap (dlsts^._2))
       (createSynsetMap (dlsts^._3))
       (createSynsetMap (dlsts^._4))
       -- (createSenseMap slists)


createLemmaMap :: [IndexItem] -> HM.HashMap Text IndexItem -- [(SenseNumber,SynsetOffset)]
createLemmaMap = HM.fromList . map (\x->(x^.idx_lemma,x))


createSynsetMap :: [DataItem] -> IM.IntMap DataItem -- ([LexItem],Text)
createSynsetMap = IM.fromList . map (\x->(x^.data_syn_offset.to unSynsetOffset,x))

{- 
createSenseMap :: [SenseItem] -> HM.HashMap (Text,Int) Int
createSenseMap _ = HM.fromList []
-- HM.fromList . map (\x->((x^.sense_sense_key.skey_lemma,x^.sense_lexid),x^.sense_ss))
-}

indexDB :: WordNetDB -> POS -> HM.HashMap Text IndexItem -- [(SenseNumber,SynsetOffset)]
indexDB w POS_N = w^.indexNounDB
indexDB w POS_V = w^.indexVerbDB
indexDB w POS_A = w^.indexAdjDB
indexDB w POS_R = w^.indexAdvDB


dataDB :: WordNetDB -> POS -> IM.IntMap DataItem -- ([LexItem],Text)
dataDB w POS_N = w^.dataNounDB
dataDB w POS_V = w^.dataVerbDB
dataDB w POS_A = w^.dataAdjDB
dataDB w POS_R = w^.dataAdvDB

{- 
senseDB :: WordNetDB -> HashMap (Text,Int) Int
senseDB w = w^.senseIdxDB
-}

parseFile :: (Text -> Maybe a) -> FilePath -> IO [Maybe a]
parseFile p fp = do
  txt <- TIO.readFile fp 
  let lst = filter (not.isComment) (T.lines txt)
  return $ map p lst


lookupLemma :: WordNetDB -> POS -> Text -> [(SenseNumber,[LexItem],Text)]
lookupLemma w p t = do
  (snum,soff) <- join .  maybeToList $ (indexDB w p) ^? at t._Just.idx_synset_offset
  (ls,desc) <- maybeToList (lookupSynset w p soff)
  return (snum,ls,desc)
   

lookupSynset :: WordNetDB -> POS -> SynsetOffset -> Maybe ([LexItem],Text)
lookupSynset w p (SynsetOffset n) = (dataDB w p) ^? at n._Just.to ((,)<$>view data_word_lex_id<*>view data_gloss)


loadDB :: FilePath -> IO WordNetDB
loadDB fp = do
  is <- (,,,) <$> (catMaybes <$> parseFile parseIndex (fp </> "index.noun"))
              <*> (catMaybes <$> parseFile parseIndex (fp </> "index.verb"))
              <*> (catMaybes <$> parseFile parseIndex (fp </> "index.adj"))
              <*> (catMaybes <$> parseFile parseIndex (fp </> "index.adv"))
  ds <- (,,,) <$> (catMaybes <$> parseFile (parseData False) (fp </> "data.noun"))
              <*> (catMaybes <$> parseFile (parseData True ) (fp </> "data.verb"))
              <*> (catMaybes <$> parseFile (parseData False) (fp </> "data.adj"))
              <*> (catMaybes <$> parseFile (parseData False) (fp </> "data.adv"))
  -- ss <- (catMaybes <$> parseFile parseSense (fp </> "index.sense"))
  return (createWordNetDB is ds {- ss -})


runSingleQuery :: String -> POS -> WordNetDB -> IO ()
runSingleQuery input typ db = do
  case decimal (T.pack input) of
    Left _str    -> queryLemma (T.pack input) typ db
    Right (n,_)  -> querySynset n typ db


queryLemma :: Text -> POS -> WordNetDB -> IO ()
queryLemma input typ db = do
  case typ of
    POS_N -> putStrLn "-- Noun --"      >> (mapM_ (TIO.putStrLn . formatLemmaSynset) $ lookupLemma db POS_N input)
    POS_V -> putStrLn "-- Verb --"      >> (mapM_ (TIO.putStrLn . formatLemmaSynset) $ lookupLemma db POS_V input)
    POS_A -> putStrLn "-- Adjective --" >> (mapM_ (TIO.putStrLn . formatLemmaSynset) $ lookupLemma db POS_A input)
    POS_R -> putStrLn "-- Adverb --"    >> (mapM_ (TIO.putStrLn . formatLemmaSynset) $ lookupLemma db POS_R input)


querySynset :: SynsetOffset -> POS -> WordNetDB -> IO ()
querySynset n typ db = do
  case typ of
    POS_N -> putStrLn "-- Noun --"      >> (mapM_ (TIO.putStrLn . formatSynset) $ lookupSynset db POS_N n)
    POS_V -> putStrLn "-- Verb --"      >> (mapM_ (TIO.putStrLn . formatSynset) $ lookupSynset db POS_V n)
    POS_A -> putStrLn "-- Adjective --" >> (mapM_ (TIO.putStrLn . formatSynset) $ lookupSynset db POS_A n)
    POS_R -> putStrLn "-- Adverb --"    >> (mapM_ (TIO.putStrLn . formatSynset) $ lookupSynset db POS_R n)

