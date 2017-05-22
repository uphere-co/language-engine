module SRL.Type where

data VArrow = Upward | Downward

data Deprel = Int

newtype DeprelPath = DeprelPath [(Deprel,VArrow)] 
