{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}


{-|
  This module combines Wikidata tagger, WikiEL.WikiEntityTagger, and CoreNLP NER.
  by discarding possiblities that both results are inconsistent.
-}
module WikiEL.WikiNamedEntityTagger 
( module WikiEL.WikiNamedEntityTagger 
, WEC.mayCite
) where

import           Data.Text                             (Text)
import           Data.Vector                           (Vector,toList,fromList,ifoldr,foldl')
import           Control.Arrow                         (second)
import qualified Data.Text                     as T
--
import           NLP.Type.NamedEntity                  (NamedEntityFrag,NamedEntityClass(Other),parseStr, _ftype,_fstr)
import           WikiEL.Type                           (EntityToken(..),IRange(..),NameUIDTable
                                                       ,PreNE(..),RelativePosition(..),WikiuidNETag)
import           WikiEL.Type.Wikidata                  (ItemID)
import           WikiEL.Misc                           (relativePos, untilNoOverlap)
import           WikiEL.WikiEntityTagger               (wikiAnnotator)
import qualified WikiEL.WikiEntityClass        as WEC


parseNERToken :: Text -> EntityToken
parseNERToken tokenStr = (\(x,y)-> EntityToken (T.dropEnd 1 x) y) $ T.breakOnEnd (T.pack "/") tokenStr


parseNEROutputStr :: Text -> [EntityToken]
parseNEROutputStr str = map parseNERToken (T.words str)


parseStanfordNE :: EntityToken -> NamedEntityFrag
parseStanfordNE (EntityToken word tag) = parseStr word tag


loadStanfordNERoutput :: Text -> [NamedEntityFrag]
loadStanfordNERoutput content = map parseStanfordNE (parseNEROutputStr content)


getWords :: [NamedEntityFrag] -> [Text]
getWords  = map _fstr


getNETags :: [NamedEntityFrag] -> [NamedEntityClass]
getNETags = map _ftype




    

