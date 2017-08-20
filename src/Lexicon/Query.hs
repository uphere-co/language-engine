{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexicon.Query where

import           Control.Applicative
import           Control.Lens
import           Data.Function                (on)
import           Data.Hashable                (Hashable)
import           Data.List                    (groupBy)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO
import           Data.Text.Read               (decimal)
--
import           Lexicon.Mapping.Type


parseRoleInst :: [Text] -> RoleInstance
parseRoleInst (i:lma:sense:frame:rest)
  = let lst = map (\w -> let x:y:_ = T.splitOn ":" w in (x,y)) rest
    in ((lma,V,sense),("frame",frame):lst)
parseRoleInst x = error ("parseRoleInstance: " ++ show x)


loadRoleInsts :: FilePath -> IO [RoleInstance]
loadRoleInsts fp = map parseRoleInst . map T.words . T.lines <$> T.IO.readFile fp


convertONIDtoSenseID lma sense_num =
  if lma == "hold" && sense_num == "5"       -- this is an ad hoc treatment for group 2
  then (lma,V,"2." <> sense_num)
  else (lma,V,"1." <> sense_num)


parseWithNullCheck :: (Text -> a) -> Text -> Maybe a
parseWithNullCheck f w = if w == "null" then Nothing else Just (f w)


parseRolePattInst
  :: (Read p) =>
     [Text] -> (Text,Text,Maybe p,Maybe Text,Maybe Text,Maybe Text,Maybe Text,Maybe Text,Int)
parseRolePattInst ws@[lma,sense,mvoice,marg0,marg1,marg2,marg3,marg4,count] =
  ( lma ,sense
  , parseWithNullCheck (read . T.unpack) mvoice
  , parseWithNullCheck id marg0
  , parseWithNullCheck id marg1
  , parseWithNullCheck id marg2
  , parseWithNullCheck id marg3
  , parseWithNullCheck id marg4
  , either (error ("error: " ++ show ws)) fst (decimal count)
  )




loadRolePattInsts :: (Read v,Hashable v) => FilePath -> IO [RolePattInstance v]
loadRolePattInsts fp = do
  txt <- T.IO.readFile fp
  let getLemmaSense x = (x^._1,V,x^._2)
      getArgTable x = ArgPattern (x^._3) (x^._4) (x^._5) (x^._6) (x^._7) (x^._8)
  return . map (\xs  -> (getLemmaSense (head xs),map (\x->(getArgTable x,x^._9)) xs))
         . groupBy ((==) `on` getLemmaSense)
         . map parseRolePattInst
         . map T.words
         . T.lines
         $ txt
