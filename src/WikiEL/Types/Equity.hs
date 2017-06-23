{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Types.Equity where

import           Data.Text                             (Text)


newtype Symbol = Symbol { _symbol :: Text }
               deriving (Eq, Ord, Show)
newtype Exchange = Exchange { _exchange :: Text }
                 deriving (Eq, Ord, Show)
data EquityTicker = EquityTicker Exchange Symbol 
                  deriving (Eq, Ord)

instance Show EquityTicker where
  show (EquityTicker e s) = show (_exchange e) ++ ":" ++ show (_symbol s)


newtype GICS = GICS { _gics :: Text }
             deriving (Eq, Ord)

instance Show GICS where
  show (GICS sector) = "GICS:" ++ show sector

newtype GICSsub = GICSsub { _gicsSub :: Text }
             deriving (Eq, Ord)

instance Show GICSsub where
  show (GICSsub sector) = "GICS_sub:" ++ show sector
