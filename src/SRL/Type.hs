{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module SRL.Type where

import           Control.Lens     hiding (Level)
import           Data.Maybe              (mapMaybe)
import           Data.Text               (Text)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as CS
import qualified CoreNLP.Simple.Type.Simplified        as S
import           NLP.Type.PennTreebankII
import           NLP.Type.TreeZipper
import           NLP.Type.UniversalDependencies2.Syntax
import           PropBank.Type.Prop


data Position = Before | After | Embed
              deriving (Show,Eq,Ord,Enum,Bounded)

data Direction = Up | Down
               deriving (Show,Eq,Ord,Enum,Bounded)

type ParseTreePath = [(Either ChunkTag POSTag, Direction)]

data Voice = Active | Passive
           deriving (Show,Eq,Ord,Enum,Bounded)

type TreeICP a = Tree (Range,ChunkTag) (Int,(POSTag,a))

type TreeZipperICP a = TreeZipper (Range,ChunkTag) (Int,(POSTag,a))

type RoleSet = (Text,Text)

type Level = Int

data DepInfo = DepInfo { _dinfo_self :: Int
                       , _dinfo_mother :: Int
                       , _dinfo_rel :: DependencyRelation
                       , _dinfo_level :: Maybe Level }
               deriving (Show,Eq,Ord)


makeLenses ''DepInfo

data SRLFeature = SRLFeat { _sfeat_range :: Range
                          , _sfeat_ptp :: ParseTreePath
                          , _sfeat_depRelPath :: ListZipper DepInfo
                          , _sfeat_headword :: Maybe (Int,(Level,(POSTag,Text)))
                          }
                deriving (Show)

makeLenses ''SRLFeature                         


data ArgNodeFeature = AFeat { _afeat_label :: PropBankLabel
                            , _afeat_srlfeature :: SRLFeature --  (Range,ParseTreePath,Maybe (Int,(Level,(POSTag,Text)))))
                            }
                    deriving (Show)

makeLenses ''ArgNodeFeature


data InstanceFeature = IFeat { _ifeat_predidx :: Int
                             , _ifeat_rolesetid :: RoleSet
                             , _ifeat_voice :: Voice
                             , _ifeat_afeatss :: [[ArgNodeFeature]]
                             }
                     deriving (Show)

--   (Int,RoleSet,Voice, [[ArgNodeFeature]])
makeLenses ''InstanceFeature



data SentenceInfo = SentInfo { _corenlp_sent :: CS.Sentence
                             , _corenlp_tree :: PennTree
                             , _corenlp_dep  :: S.Dependency
                             }
                  deriving Show

makeLenses ''SentenceInfo


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

data NodeRange = Single Range
               | Multi [Range]
               deriving (Show)

data ArgumentInput = ArgumentInput { _pblabel :: PropBankLabel
                                   , _nodes :: [NodeRange]
                                   }
                   deriving (Show)
                            
makeLenses ''ArgumentInput


argumentInputFromMatchedArgument :: MatchedArgument -> ArgumentInput
argumentInputFromMatchedArgument arg =
  let label = arg^.ma_argument.arg_label
      rs = map (toListOf (mn_trees.traverse._1)) (arg^.ma_nodes)
      rs' = flip mapMaybe rs $ \r -> case r of
                                       []  -> Nothing
                                       [x] -> Just (Single x)
                                       xs  -> Just (Multi xs)
  in ArgumentInput label rs'


data InstanceInput = InstanceInput { _predicate_id :: Int
                                   , _lemma_roleset_id :: (Text,Text)
                                   , _argument_inputs :: [ArgumentInput]
                                   }
                   deriving (Show)

makeLenses ''InstanceInput

isPriorTo :: Range -> Range -> Bool
(_b1,e1) `isPriorTo` (b2,_e2) = e1 < b2


isAfter :: Range -> Range -> Bool
r1 `isAfter` r2 = r2 `isPriorTo` r1

isNotOverlappedWith :: Range -> Range -> Bool
r1 `isNotOverlappedWith` r2 = r1 `isPriorTo` r2 || r1 `isAfter` r2

position :: Int -> Range -> Position
position n (b,e) = if | n < b     -> Before
                      | n > e     -> After
                      | otherwise -> Embed

-- | duplicate of comonad is dual to join of monad, i.e. duplicate :: w a -> w (w a)
--   In Tree case, we can make a tree where each node is the subtree at the node point using duplicate 
duplicate :: Tree c a -> Tree (Tree c a) (Tree c a)
duplicate (PN x xs) = PN (PN x xs) (map duplicate xs)
duplicate (PL x) = PL (PL x)


