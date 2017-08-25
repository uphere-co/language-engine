{-# LANGUAGE OverloadedStrings #-}

module Lexicon.Format where

import           Control.Lens   ((^.))
import           Data.Maybe     (fromMaybe)
import           Data.Text      (Text)
import           Text.Printf    (printf)
--
import           Lexicon.Type   (ArgPattern(..),patt_property
                                ,patt_arg0,patt_arg1,patt_arg2,patt_arg3,patt_arg4)


formatArgPatt :: (Show a) => Text -> ArgPattern a Text -> String
formatArgPatt propname patt =
  printf "%-5s:%-15s arg0: %-10s   arg1: %-10s   arg2: %-10s   arg3: %-10s   arg4: %-10s"
    propname
    (maybe "unidentified" show (patt^.patt_property))
    (fromMaybe "" (patt^.patt_arg0))
    (fromMaybe "" (patt^.patt_arg1))
    (fromMaybe "" (patt^.patt_arg2))
    (fromMaybe "" (patt^.patt_arg3))
    (fromMaybe "" (patt^.patt_arg4))


formatRoleMap :: [(Text,Text)] -> String
formatRoleMap rolemap =
  printf "                      arg0: %-10s   arg1: %-10s   arg2: %-10s   arg3: %-10s   arg4: %-10s"
    (fromMaybe "" (lookup "arg0" rolemap))
    (fromMaybe "" (lookup "arg1" rolemap))
    (fromMaybe "" (lookup "arg2" rolemap))
    (fromMaybe "" (lookup "arg3" rolemap))
    (fromMaybe "" (lookup "arg4" rolemap))
