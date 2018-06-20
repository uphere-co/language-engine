{-# LANGUAGE OverloadedStrings #-}

module Lexicon.Format where

import           Control.Lens   ((^.))
import           Data.List      (partition)
import           Data.Maybe     (fromMaybe)
import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import qualified Data.Text as T
import           Formatting     ((%),(%.),sformat,stext,left,right,int)
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


--   "%-5s:%-15s arg0: %-10s   arg1: %-10s   arg2: %-10s   arg3: %-10s   arg4: %-10s"
formatArgPatt :: (Show a) => Text -> ArgPattern a GRel -> Text
formatArgPatt propname patt =
    sformat
      (   ls 5 % ":" % ls 15
        %   " arg0: " % ls 10
        % "   arg1: " % ls 10
        % "   arg2: " % ls 10
        % "   arg3: " % ls 10
        % "   arg4: " % ls 10)
      propname
      (maybe "unidentified" (T.pack . show) (patt^.patt_property))
      (maybe "" formatGRel (patt^.patt_arg0))
      (maybe "" formatGRel (patt^.patt_arg1))
      (maybe "" formatGRel (patt^.patt_arg2))
      (maybe "" formatGRel (patt^.patt_arg3))
      (maybe "" formatGRel (patt^.patt_arg4))
  where
    ls n = left n ' ' %. stext

--   "%s     #count: %5d"
formatArgPattStat :: (Show p) => [(ArgPattern p GRel,Int)] -> Text
formatArgPattStat pstats =
  T.intercalate "\n" $ flip map pstats $ \(patt,n) ->
    sformat
      (stext % "     #count: " % (right 5 ' ' %. int))
      (formatArgPatt "voice" patt)
      (n :: Int)


--   "                      arg0: %-10s   arg1: %-10s   arg2: %-10s   arg3: %-10s   arg4: %-10s"
formatRoleMap :: [(Text,Text)] -> Text
formatRoleMap rolemap =
    sformat
      (   "                      arg0: " % ls 10
        %                    "   arg1: " % ls 10
        %                    "   arg2: " % ls 10
        %                    "   arg3: " % ls 10
        %                    "   arg4: " % ls 10)
      (fromMaybe "" (lookup "arg0" rolemap))
      (fromMaybe "" (lookup "arg1" rolemap))
      (fromMaybe "" (lookup "arg2" rolemap))
      (fromMaybe "" (lookup "arg3" rolemap))
      (fromMaybe "" (lookup "arg4" rolemap))
  where
    ls n = left n ' ' %. stext

formatRoleMapTSV :: (Int,RoleInstance) -> Text
formatRoleMapTSV (i,((lma,_,sense),m)) =
  let ([(_,frame)],rest) = partition (\(k,_) -> k == "frame") m
      txt = T.intercalate "\t" (map (\(k,v) -> sformat (stext % ":" % stext) k v) rest)
  in sformat
       (int % "\t" % stext % "\t" % stext % "\t" % stext % "\t" % stext % " \n")
       i
       lma
       sense
       frame
       txt
