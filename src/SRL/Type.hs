module SRL.Type where

import SRL.CoNLL.CoNLL08.Type

data VArrow = Upward | Downward

newtype DeprelPath = DeprelPath [(Deprel,VArrow)] 
