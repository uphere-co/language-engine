module OntoNotes.Format where

import           Control.Lens
import           Data.Maybe
import           Text.Printf
--
import OntoNotes.Type.ArgTable

formatArgPatt :: ArgPattern -> String
formatArgPatt patt = printf "voice:%-15s arg0: %-10s   arg1: %-10s   arg2: %-10s   arg3: %-10s   arg4: %-10s"
                       (maybe "unidentified" show (patt^.patt_voice))
                       (fromMaybe "" (patt^.patt_arg0))
                       (fromMaybe "" (patt^.patt_arg1))
                       (fromMaybe "" (patt^.patt_arg2))
                       (fromMaybe "" (patt^.patt_arg3))
                       (fromMaybe "" (patt^.patt_arg4))
