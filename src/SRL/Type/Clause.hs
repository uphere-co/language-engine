module SRL.Type.Clause where

import           Data.Text                              (Text)
--
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
--
                            

data SBARType = SB_Word (POSTag,Text)
              | SB_WH   N.PhraseTag
              | SB_None
              deriving Show


data STag = S_RT
          | S_SBAR SBARType
          | S_CL N.ClauseTag
          | S_VP [(Int,(POSTag,Text))]
          | S_PP Text
          | S_OTHER N.PhraseTag
          deriving Show

