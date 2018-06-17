{-# LANGUAGE TemplateHaskell #-}

module PropBank.Type.Match where

import           Control.Lens
--
import           NLP.Type.PennTreebankII
--
import           PropBank.Type.Prop


data MatchedArgNode
  = MatchedArgNode { _mn_node :: (Range,Node)
                   , _mn_trees :: [(Range,PennTreeIdx)]
                   }
  deriving Show

makeLenses ''MatchedArgNode

data MatchedArgument
  = MatchedArgument { _ma_argument :: Argument
                    , _ma_nodes :: [MatchedArgNode]
                    }
  deriving Show

makeLenses ''MatchedArgument

data MatchedInstance
  = MatchedInstance { _mi_instance :: Instance
                    , _mi_arguments :: [MatchedArgument]
                    }
  deriving Show

makeLenses ''MatchedInstance
