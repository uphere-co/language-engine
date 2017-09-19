{-# LANGUAGE OverloadedStrings #-}

module Lexicon.Format where

import           Control.Lens   ((^.))
import           Data.List      (intercalate,partition)
import           Data.Maybe     (fromMaybe)
import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import           Text.Printf    (printf)
--
import           Lexicon.Type   (ArgPattern(..),GArg(..),GRel(..),RoleInstance
                                ,patt_property,patt_arg0,patt_arg1,patt_arg2,patt_arg3,patt_arg4)


formatGArg :: GArg -> Text
formatGArg GASBJ = "SBJ"
formatGArg GA1   = "1"
formatGArg GA2   = "2"


formatGRel :: GRel -> Text
formatGRel (GR_NP   mgarg) = "NP"   <> maybe "" (\a -> "-" <> formatGArg a) mgarg
formatGRel (GR_S    mgarg) = "S"    <> maybe "" (\a -> "-" <> formatGArg a) mgarg
formatGRel (GR_SBAR mgarg) = "SBAR" <> maybe "" (\a -> "-" <> formatGArg a) mgarg
formatGRel (GR_PP   mparg) = "PP"   <> maybe "" (\(p,b) -> "-" <> p <> if b then "-ing" else "") mparg
formatGRel (GR_ADVP mtxt)  = "ADVP" <> maybe "" (\a -> "-" <> a) mtxt
formatGRel GR_ADJP         = "ADJP"
formatGRel (GR_X    txt)   = "??"   <> "(" <> txt <> ")"


formatArgPatt :: (Show a) => Text -> ArgPattern a GRel -> String
formatArgPatt propname patt =
  printf "%-5s:%-15s arg0: %-10s   arg1: %-10s   arg2: %-10s   arg3: %-10s   arg4: %-10s"
    propname
    (maybe "unidentified" show (patt^.patt_property))
    (maybe "" formatGRel (patt^.patt_arg0))
    (maybe "" formatGRel (patt^.patt_arg1))
    (maybe "" formatGRel (patt^.patt_arg2))
    (maybe "" formatGRel (patt^.patt_arg3))
    (maybe "" formatGRel (patt^.patt_arg4))


formatArgPattStat :: (Show p) => [(ArgPattern p GRel,Int)] -> String
formatArgPattStat pstats =
  intercalate "\n" $ flip map pstats $ \(patt,n) ->
    printf "%s     #count: %5d" (formatArgPatt "voice" patt) (n :: Int)



formatRoleMap :: [(Text,Text)] -> String
formatRoleMap rolemap =
  printf "                      arg0: %-10s   arg1: %-10s   arg2: %-10s   arg3: %-10s   arg4: %-10s"
    (fromMaybe "" (lookup "arg0" rolemap))
    (fromMaybe "" (lookup "arg1" rolemap))
    (fromMaybe "" (lookup "arg2" rolemap))
    (fromMaybe "" (lookup "arg3" rolemap))
    (fromMaybe "" (lookup "arg4" rolemap))


formatRoleMapTSV :: (Int,RoleInstance) -> String
formatRoleMapTSV (i,((lma,_,sense),m)) =
  let ([(_,frame)],rest) = partition (\(k,_) -> k == "frame") m
      txt = intercalate "\t" (map (\(k,v) -> printf "%s:%s" k v) rest) :: String
  in printf "%d\t%s\t%s\t%s\t%s\n" i lma sense frame txt
